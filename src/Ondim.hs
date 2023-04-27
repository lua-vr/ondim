{-# LANGUAGE RankNTypes #-}

{- | This module defines a user-friendly API over the core functionality
 (implemented in Ondim.MultiWalk.Core).
-}
module Ondim
  ( -- * Classes
    OndimNode (..),

    -- * Combinators
    module Ondim.MultiWalk.Combinators,

    -- * Data types
    GlobalConstraints,
    Expansion,
    GlobalExpansion,
    SomeExpansion,
    someExpansion,
    globalExpansion,
    textData,
    namespace,
    Expansions,
    splitExpansionKey,
    lookupExpansion,
    insertExpansion,
    deleteExpansion,
    Filter,
    GlobalFilter,
    SomeFilter,
    someFilter,
    globalFilter,
    Filters,
    OndimState (..),

    -- * Monad
    Ondim,
    runOndimTWith,
    evalOndimTWith,
    evalOndimT,

    -- * Exceptions
    OndimException (..),
    throwNotBound,
    throwCustom,

    -- * State transformations

    -- Expansions
    unbind,
    ExpansionMap,
    (##),
    (#@),
    (#*),
    (#.),
    (#:),
    binding,
    withExpansion,
    withExpansions,
    withoutExpansions,
    putExpansion,
    getExpansion,
    getSomeExpansion,
    getTextData,
    callExpansion,
    callText,
    -- Filters
    FilterMap,
    ($#),
    ($*),
    ($:),
    bindingFilters,
    withFilter,
    withFilters,
    withoutFilters,
    -- State manipulation
    getOndimS,
    putOndimS,
    modifyOndimS,

    -- * Node lifting
    liftNode,
    liftNodes,
    modSubLift,
    liftSubstructures,
    fromTemplate,

    -- * Structure
    getSubstructure,
    modSubstructureM,
    children,
    liftChildren,
    substructureAttributes,
    lookupAttr,

    -- * Auxiliary
    AllMods,
    Substructure,
    Attribute,
  )
where

import Control.Monad.Writer.CPS
import Control.MultiWalk.HasSub (AllMods)
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Map qualified as Map
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import Ondim.MultiWalk.Combinators
import Ondim.MultiWalk.Core
import Ondim.MultiWalk.Substructure
import Type.Reflection (typeRep)
import Prelude hiding (All)

-- | Runs the Ondim action with a given initial state.
evalOndimTWith ::
  Monad m =>
  OndimState m ->
  Ondim m a ->
  m (Either OndimException a)
evalOndimTWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialGS
      `evalStateT` s

runOndimTWith ::
  Monad m =>
  OndimState m ->
  Ondim m a ->
  m (Either OndimException (a, OndimState m))
runOndimTWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialGS
      `runStateT` s

-- | Runs the Ondim action with empty initial state.
evalOndimT :: Monad m => Ondim m a -> m (Either OndimException a)
evalOndimT = evalOndimTWith mempty

-- State manipulation

getOndimS :: Monad m => Ondim m (OndimState m)
getOndimS = Ondim get

modifyOndimS :: Monad m => (OndimState m -> OndimState m) -> Ondim m ()
modifyOndimS = Ondim . modify'

putOndimS :: Monad m => OndimState m -> Ondim m ()
putOndimS = Ondim . put

withExpansion ::
  Monad m =>
  Text ->
  Maybe (SomeExpansion m) ->
  Ondim m a ->
  Ondim m a
withExpansion name ex st = do
  pEx <- Ondim $ gets (lookupExpansion name . expansions)
  Ondim $ modify' \s -> s {expansions = insOrDel ex (expansions s)}
  st <* modifyOndimS \s -> s {expansions = insOrDel pEx (expansions s)}
  where
    insOrDel = maybe (deleteExpansion name) (insertExpansion name)

withFilter ::
  Monad m =>
  Text ->
  Maybe (SomeFilter m) ->
  Ondim m a ->
  Ondim m a
withFilter name ex st = do
  pEx <- Ondim $ gets (Map.lookup name . filters)
  Ondim $ modify' \s -> s {filters = insOrDel ex (filters s)}
  st <* modifyOndimS \s -> s {filters = insOrDel pEx (filters s)}
  where
    insOrDel x = Map.alter (const x) name

-- | "Bind" new expansions locally.
withExpansions :: Monad m => Expansions m -> Ondim m a -> Ondim m a
withExpansions (Expansions exps) o = foldr (\(k, v) -> withExpansion k (Just v)) o (HMap.toList exps)

-- | "Bind" filters locally.
withFilters :: Monad m => Filters m -> Ondim m a -> Ondim m a
withFilters filt o = foldr (\(k, v) -> withFilter k (Just v)) o (Map.toList filt)

-- | "Unbind" many expansions locally.
withoutExpansions :: Monad m => [Text] -> Ondim m a -> Ondim m a
withoutExpansions names o = foldr (`withExpansion` Nothing) o names

-- | "Unbind" many expansions locally.
withoutFilters :: Monad m => [Text] -> Ondim m a -> Ondim m a
withoutFilters names o = foldr (`withFilter` Nothing) o names

-- | Put a new expansion into the local state, modifying the scope.
putExpansion :: Monad m => Text -> SomeExpansion m -> Ondim m ()
putExpansion key ex =
  modifyOndimS \s -> s {expansions = insertExpansion key ex (expansions s)}

type ExpansionMap m = Writer [(Text, Maybe (SomeExpansion m))] ()

infixr 0 #<>

(#<>) :: Text -> m -> Writer [(Text, m)] ()
name #<> ex = tell [(name, ex)]

unbind :: Text -> Writer [(Text, Maybe m)] ()
unbind k = k #<> Nothing

infixr 0 #:

(#:) :: Text -> SomeExpansion m -> ExpansionMap m
name #: ex = name #<> Just ex

someExpansion :: Typeable t => Expansion m t -> SomeExpansion m
someExpansion = SomeExpansion typeRep

infixr 0 ##

(##) :: Typeable t => Text -> Expansion m t -> ExpansionMap m
name ## ex = name #: someExpansion ex

textData :: Text -> SomeExpansion m
textData = TextData

infixr 0 #@

(#@) :: Text -> Text -> ExpansionMap m
name #@ ex = name #: TextData ex

globalExpansion :: GlobalExpansion m -> SomeExpansion m
globalExpansion = GlobalExpansion

infixr 0 #*

(#*) :: Text -> GlobalExpansion m -> ExpansionMap m
name #* ex = name #: globalExpansion ex

namespace :: ExpansionMap m -> SomeExpansion m
namespace ex = Namespace $ foldl' go mempty exps
  where
    go = flip $ uncurry insertExpansion
    exps = mapMaybe sequence $ execWriter ex

infixr 0 #.

(#.) :: Text -> ExpansionMap m -> ExpansionMap m
name #. ex = name #: namespace ex

type FilterMap m = Writer [(Text, Maybe (SomeFilter m))] ()

infixr 0 $:

($:) :: Text -> SomeFilter m -> FilterMap m
name $: ex = name #<> Just ex

someFilter :: Typeable t => Filter m t -> SomeFilter m
someFilter = SomeFilter typeRep

infixr 0 $#

($#) :: Typeable t => Text -> Filter m t -> FilterMap m
name $# ex = name $: someFilter ex

globalFilter :: GlobalFilter m -> SomeFilter m
globalFilter = GlobalFilter

infixr 0 $*

($*) :: Text -> GlobalFilter m -> FilterMap m
name $* ex = name $: globalFilter ex

-- | Infix version of @withExpansions@ to bind using MapSyntax.
binding ::
  Monad m =>
  Ondim m a ->
  ExpansionMap m ->
  Ondim m a
binding o exps =
  let kvs = execWriter exps
   in foldl' (flip $ uncurry withExpansion) o kvs

-- | Infix version of @withFilters@ to bind using MapSyntax.
bindingFilters ::
  Monad m =>
  Ondim m a ->
  FilterMap m ->
  Ondim m a
bindingFilters o filts =
  let kvs = execWriter filts
   in foldl' (flip $ uncurry withFilter) o kvs

-- Children

children ::
  forall t.
  ( OndimNode t
  ) =>
  t ->
  [t]
children = getSubstructure @t

liftChildren ::
  forall t m.
  ( OndimNode t,
    Monad m
  ) =>
  Expansion m t
liftChildren = liftNodes . children

-- Attributes

-- | You can use this as a default instance for the 'attributes' class method.
substructureAttributes :: (OndimNode t, AllMods (Substructure Attribute) (ExpTypes t), Monad m) => t -> Ondim m [Attribute]
substructureAttributes = liftNodes . getSubstructure @Attribute

lookupAttr ::
  (Monad m, OndimNode t) =>
  Text ->
  t ->
  Ondim m (Maybe Text)
lookupAttr key = fmap (L.lookup key) . attributes

fromTemplate ::
  forall m t.
  ( OndimNode t,
    Monad m
  ) =>
  [t] ->
  Expansion m t
fromTemplate tpl inner =
  liftNodes tpl `binding` do
    "this.children" ## const (liftChildren inner)

-- | Either applies expansion 'name', or throws an error if it does not exist.
callExpansion :: forall t m. GlobalConstraints m t => Text -> Expansion m t
callExpansion name arg = do
  exps <- getExpansion name
  maybe (throwNotBound name) ($ arg) exps

-- | Either applies expansion 'name', or throws an error if it does not exist.
callText :: forall m. Monad m => Text -> Ondim m Text
callText name = do
  exps <- getTextData name
  maybe (throwNotBound name) pure exps
-- * Attributes

instance OndimNode Text where
  type ExpTypes Text = '[]

instance OndimNode Attribute where
  type ExpTypes Attribute = '[ToSpec (OneSub Text)]
  identify = Just . fst
