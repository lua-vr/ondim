{- | This module exports the main definitions and methods for working with Ondim.

Here, we shall call by a "node" any type that implements 'OndimNode', which
means it is compatible with the Ondim template system. Many types can be used as
nodes at once, and in fact in a single format like HTML there are multiple types
that are regarded as nodes (the HTML document, element, node and attribute are
different types implementing the class). In other formats like Pandoc the
necessity of this approach is more evident, since there are not only the @Block@
and @Inline@ types but also metadata types and so on.

Each node may have an /name/, which is simply a @Text@ string and acessed by the
'identify' class method. A node which has an name is meant to be /expanded/ by
the template system, and this is done by applying it to an 'Expansion' bound to
the same name in the local state.

Another thing specified by the 'OndimNode' instances is how different node types
are found inside a given node type, and this information is used to recursively
expand the templates in a macro-like fashion. The main sorts of "substructures"
of a node are its /children/ and /attributes/, which are acessed via the
'children' and 'attributes' methods, repectively. Since those are part of the
class, they can be used for writing expansions that don't depend on the node
type. Those polymorphic expansions are special enough to have a type alias for
them: 'PolyExpansion'. The nice thing about them is that you can write
templating code only once and expand templates of any format, while still being
able to specify expansions for a given type.

So the main function in this module is probably 'expandNode', which when applied
to a node runs the expansion machinery and turns the node into something else.

Expansions are just Haskell functions of type @t -> 'Ondim' m [t]@, and can be
defined inside Haskell code. The expansion's argument is the node whose name
matched the name of the expansion (the "caller"), and the expansion returns a
list of nodes that will replace it in the expansion process. Expansions are
bound to names in the 'Ondim' monad state, and they can also be organized into
more nested 'Namespace's. See "Ondim.State" for more information on how to
manipulate the state.

The state also store /templates/, which are essentialy values of type
@'OndimNode' t => t@ and are used to store data like text and file templates.
Every template stored in the state is also seen as an expansion, the expansion
that ignores the caller argument and returns the expanded template:

@
templateToExpansion tpl = const $ 'expandNode' tpl
@

This is not the true definition --- the caller node is not actually ignored,
instead it may be acessed from the template via the state in the @caller@
namespace. Most importantly, templates may be loaded from disk, see the module
"Ondim.Loading".

For dealing with exceptions and debug data, see the module "Ondim.Debug".
-}
module Ondim
  ( -- * Monad
    Ondim,
    runOndimWith,
    evalOndimWith,
    evalOndim,

    -- * Nodes
    OndimNode,
    identify,
    ondimCast,

    -- * Children and attributes
    children,
    expandChildren,
    lookupAttr,
    attributes,

    -- * Running templates
    expandNode,
    expandNodes,
    expandSubstructures,

    -- * Data types
    Expansion,
    PolyExpansion,

    -- * State transformations
    module Ondim.State,

    -- ** Get specific expansions
    getExpansion,
    getTemplate,
    getNamespace,
    getText,
    -- ** Calling
    callExpansion,
    callTemplate,
    callText,

    -- * Rendering
    renderNode,
    renderNodeOrError,
    renderTemplateOrError,

    -- * Auxiliary
    Attribute,
  )
where

import Data.List qualified as L
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import Ondim.MultiWalk.Core
import Ondim.State
import Prelude hiding (All)

-- | Runs the Ondim action with a given initial state.
evalOndimWith ::
  (Monad m) =>
  OndimState m ->
  Ondim m a ->
  m (Either OndimException a)
evalOndimWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialTraceData
      `evalStateT` s

-- | Runs the Ondim action with a given initial state, and also return the final state.
runOndimWith ::
  (Monad m) =>
  OndimState m ->
  Ondim m a ->
  m (Either OndimException (a, OndimState m))
runOndimWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialTraceData
      `runStateT` s

-- | Runs the Ondim action with empty initial state.
evalOndim :: (Monad m) => Ondim m a -> m (Either OndimException a)
evalOndim = evalOndimWith mempty

-- Children

{- | Returns the children of a node after expanding them.

@
'expandChildren' = 'expandNodes' . 'children'
@
-}
expandChildren ::
  forall t m.
  (OndimNode t, Monad m) =>
  Expansion m t
expandChildren = expandNodes . children

-- Attributes

-- | Lookup an attribute from a node by name.
lookupAttr ::
  (Monad m, OndimNode t) =>
  Text ->
  t ->
  Ondim m (Maybe Text)
lookupAttr key = fmap (L.lookup key) . attributes

-- | Render node as bytestring, if possible, or fail.
renderNodeOrError :: (HasCallStack, Monad m) => (OndimNode a) => a -> Ondim m LByteString
renderNodeOrError =
  case renderNode of
    Just render -> return . render
    Nothing -> const $ throwTemplateError "This type cannot be rendered."

-- | Expand and then render template called 'name' to bytestring.
renderTemplateOrError :: (HasCallStack, Monad m) => Text -> Ondim m LByteString
renderTemplateOrError name = do
  mbValue <- Ondim $ gets (lookup name . expansions)
  case mbValue of
    Just (Template _ site thing) ->
      renderNodeOrError
        =<< withSite site (expandSubstructures thing)
    Just _ -> throwExpFailure @() name (FailureOther "Identifier not bound to a template.")
    Nothing -> throwExpFailure @() name NotBound

-- | Either applies template 'name', or throws a failure if it does not exist.
callTemplate :: forall t m. (OndimNode t, Monad m) => Text -> Ondim m [t]
callTemplate name = do
  exps <- getTemplate name
  either (throwExpFailure @t name) return exps

-- | Either applies text 'name', or throws a failure if it does not exist.
callText :: (Monad m) => Text -> Ondim m Text
callText name = do
  exps <- getText name
  either (throwExpFailure @Text name) return exps

-- | Either applies expansion 'name', or throws a failure if it does not exist.
callExpansion :: forall t m. (OndimNode t, Monad m) => Text -> Expansion m t
callExpansion name arg = do
  exps <- getExpansion name
  either (throwExpFailure @t name) ($ arg) exps
