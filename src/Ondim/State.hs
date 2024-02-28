{-# LANGUAGE RankNTypes #-}

module Ondim.State
  ( -- * Namespace maps
    NamespaceMap,
    binding,
    mapToNamespace,
    (#:),
    unbind,

    -- ** Typed expansions
    (##),
    typedExpansion,
    typedExpansion',

    -- ** Polymorphic expansions
    (#*),
    polyExpansion,
    polyExpansion',

    -- ** Templates
    (#%),
    templateData,
    templateData',

    -- ** Textual templates
    (#@),
    textData,
    textData',

    -- ** Namespaces
    (#.),
    namespace,
    namespace',

    -- * Datatypes
    OndimState (..),
    NamespaceItem,
    Namespace,

    -- ** Manipulate the whole state
    getOndimS,
    modifyOndimS,
    putOndimS,

    -- ** Manipulate the state by keys
    withSomeExpansion,
    putSomeExpansion,
    withoutExpansions,
    withNamespace,

    -- ** Altering namespaces
    lookupExpansion,
    insertExpansion,
    deleteExpansion,
  )
where

import Data.Char (isLetter)
import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as T
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class (OndimNode)
import Type.Reflection (typeRep)

-- * User API

-- State manipulation

-- | Get the Ondim state.
getOndimS :: (Monad m) => Ondim m (OndimState m)
getOndimS = Ondim get

modifyOndimS :: (Monad m) => (OndimState m -> OndimState m) -> Ondim m ()
modifyOndimS = Ondim . modify'

putOndimS :: (Monad m) => OndimState m -> Ondim m ()
putOndimS = Ondim . put

-- | Either bind or unbind an expansion locally.
withSomeExpansion ::
  (Monad m) =>
  Text ->
  Maybe (NamespaceItem m) ->
  Ondim m a ->
  Ondim m a
withSomeExpansion name ex st = do
  pEx <- Ondim $ gets (lookupExpansion name . expansions)
  Ondim $ modify' \s -> s {expansions = insOrDel ex (expansions s)}
  st <* modifyOndimS \s -> s {expansions = insOrDel pEx (expansions s)}
  where
    insOrDel = maybe (deleteExpansion name) (insertExpansion name)

-- | Bind a namespace locally.
withNamespace :: (Monad m) => Namespace m -> Ondim m a -> Ondim m a
withNamespace (Namespace exps) o =
  foldr (\(k, v) -> withSomeExpansion k (Just v)) o (HMap.toList exps)

-- | Unbind a list of expansions locally.
withoutExpansions :: (Monad m) => [Text] -> Ondim m a -> Ondim m a
withoutExpansions names o = foldr (`withSomeExpansion` Nothing) o names

-- | Either put or delete an expansion from the state.
putSomeExpansion :: (Monad m) => Text -> Maybe (NamespaceItem m) -> Ondim m ()
putSomeExpansion name ex =
  modifyOndimS \s -> s {expansions = insOrDel ex (expansions s)}
  where
    insOrDel = maybe (deleteExpansion name) (insertExpansion name)

infixr 0 #<>

(#<>) :: Text -> Maybe (NamespaceItem m) -> NamespaceMap m
name #<> ex = NamespaceMapM $ modify' ((name, ex) :)

-- | Remove a previously added item from the 'NamespaceMap'.
unbind :: Text -> NamespaceMap m
unbind k = k #<> Nothing

-- Expansions

newtype NamespaceMapM m a = NamespaceMapM (State [(Text, Maybe (NamespaceItem m))] a)
  deriving newtype (Functor, Applicative, Monad)

{- | 'NamespaceMap' provides a monad interface for defining namespaces
   declaratively. You should enable the @BlockArguments@ extension when using
   it. For instance:

@
'Ondim.expandChildren' myNode
  \`'binding'\` do
    "something" '##' myTypedExp
    "is-cow" '#*' ifElse isCow
    "name" '#@' \"Joana\"
    "properties" '#.' do
      "age" '#@' show age
      "state" '#@' "hungry"
@
-}
type NamespaceMap m = NamespaceMapM m ()

infixr 0 #:

-- | Infix to add a 'NamespaceItem' to a 'NamespaceMap'.
(#:) :: Text -> NamespaceItem m -> NamespaceMap m
name #: ex = name #<> Just ex

typedExpansion :: (HasCallStack, Typeable t) => Expansion m t -> NamespaceItem m
typedExpansion = TypedExpansion typeRep callStackSite

typedExpansion' :: (Typeable t) => DefinitionSite -> Expansion m t -> NamespaceItem m
typedExpansion' = TypedExpansion typeRep

infixr 0 ##

{- | Infix to add an 'Expansion' to a 'NamespaceMap'.

@
name '##' expansion = name '#:' 'typedExpansion' expansion
@
-}
(##) :: (HasCallStack, Typeable t) => Text -> Expansion m t -> NamespaceMap m
name ## ex = name #: typedExpansion ex

templateData :: forall a m. (HasCallStack, OndimNode a) => a -> NamespaceItem m
templateData = Template typeRep callStackSite

templateData' :: forall a m. (HasCallStack, OndimNode a) => DefinitionSite -> a -> NamespaceItem m
templateData' = Template typeRep

infixr 0 #%

{- | Infix to add a template (any type with an 'OndimNode' instance) to a
   'NamespaceMap'.

@
name '#%' template = name '#:' 'templateData' template
@
-}
(#%) :: (HasCallStack, OndimNode a) => Text -> a -> NamespaceMap m
name #% ex = name #: templateData ex

textData :: (HasCallStack) => Text -> NamespaceItem m
textData = templateData

textData' :: DefinitionSite -> Text -> NamespaceItem m
textData' = templateData'

infixr 0 #@

{- | Infix to add a textual data to a 'NamespaceMap'. Just a specialized version
   of '#%'.

@
name '#@' text = name '#:' 'textData' text
@
-}
(#@) :: (HasCallStack) => Text -> Text -> NamespaceMap m
(#@) = (#%)

polyExpansion :: (HasCallStack) => PolyExpansion m -> NamespaceItem m
polyExpansion = PolyExpansion callStackSite

polyExpansion' :: DefinitionSite -> PolyExpansion m -> NamespaceItem m
polyExpansion' = PolyExpansion

infixr 0 #*

{- | Infix to add a t'PolyExpansion' to a 'NamespaceMap'.

@
name '#*' expansion = name '#:' 'polyExpansion' expansion
@
-}
(#*) :: (HasCallStack) => Text -> PolyExpansion m -> NamespaceMap m
name #* ex = name #: polyExpansion ex

-- | Runs the 'NamespaceMap' monad to get a 'Namespace'.
mapToNamespace :: NamespaceMap m -> Namespace m
mapToNamespace (NamespaceMapM ex) = foldr go mempty exps
  where
    go = uncurry insertExpansion
    exps = mapMaybe sequence $ execState ex []

namespace :: NamespaceMap m -> NamespaceItem m
namespace = NamespaceData . mapToNamespace

namespace' :: Namespace m -> NamespaceItem m
namespace' = NamespaceData

infixr 0 #.

{- | Infix to nest a 'NamespaceMap' inside a 'NamespaceMap'.

@
name '#.' nsMap = name '#:' 'namespace' nsMap
@
-}
(#.) :: Text -> NamespaceMap m -> NamespaceMap m
name #. ex = name #: namespace ex

{- | Infix version of 'withNamespace' meant to bind expansions more conveniently
   by using 'NamespaceMap's.
-}
binding ::
  (Monad m) =>
  Ondim m a ->
  NamespaceMap m ->
  Ondim m a
binding o (NamespaceMapM exps) =
  let kvs = execState exps []
   in foldl' (flip $ uncurry withSomeExpansion) o kvs

splitExpansionKey :: Text -> [Text]
splitExpansionKey = T.split (\c -> c /= '-' && not (isLetter c))

lookupExpansion' :: [Text] -> Namespace m -> Maybe (NamespaceItem m)
lookupExpansion' keys (Namespace e) = go keys e
  where
    go [] _ = Nothing
    go [k] m = HMap.lookup k m
    go (k : ks) m = case HMap.lookup k m of
      Just (NamespaceData (Namespace n)) -> go ks n
      Just {} -> Nothing
      Nothing -> Nothing

lookupExpansion :: Text -> Namespace m -> Maybe (NamespaceItem m)
lookupExpansion = lookupExpansion' . splitExpansionKey

insertExpansion' :: [Text] -> NamespaceItem m -> Namespace m -> Namespace m
insertExpansion' keys e (Namespace es) = Namespace $ go keys es
  where
    go [] = id
    go [k] = HMap.insert k e
    go (k : ks) =
      flip HMap.alter k $
        Just
          . NamespaceData
          . Namespace
          . \case
            Just (NamespaceData (Namespace n)) -> go ks n
            _notNamespace -> go ks mempty

insertExpansion :: Text -> NamespaceItem m -> Namespace m -> Namespace m
insertExpansion = insertExpansion' . splitExpansionKey

deleteExpansion' :: [Text] -> Namespace m -> Namespace m
deleteExpansion' keys v@(Namespace es) =
  case keys of
    [] -> v
    [k] -> Namespace $ HMap.delete k es
    (k : ks) -> Namespace $ go ks k es
  where
    go ks = HMap.update \case
      (NamespaceData n) ->
        case deleteExpansion' ks n of
          x@(Namespace hmap)
            | HMap.null hmap -> Nothing
            | otherwise -> Just $ NamespaceData x
      x -> Just x

deleteExpansion :: Text -> Namespace m -> Namespace m
deleteExpansion = deleteExpansion' . splitExpansionKey
