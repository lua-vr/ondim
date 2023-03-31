{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- | This module defines a user-friendly API over the core functionality
 (implemented in Ondim.MultiWalk.Core).
-}
module Ondim
  ( -- * Classes
    OndimNode (..),

    -- * HasSub
    HasSub (..),
    GSubTag,
    -- Spec
    SubSpec (..),
    BuildSpec,
    ToSpecList,
    -- Combinators
    Under,
    MatchWith,

    -- * Data types
    GlobalConstraints,
    Expansion,
    GlobalExpansion,
    SomeExpansion (..),
    toSomeExpansion,
    Expansions (..),
    splitExpansionKey,
    lookupExpansion,
    insertExpansion,
    deleteExpansion,
    Filter,
    GlobalFilter,
    SomeFilter (..),
    toSomeFilter,
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
    binding,
    withExpansion,
    withExpansions,
    withoutExpansions,
    putExpansion,
    getExpansion,
    getTextData,
    callExpansion,
    callText,
    -- Filters
    FilterMap,
    ($#),
    ($*),
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
    modSubstructure,
    modSubstructureM,
    children,
    liftChildren,
    attributes,
    lookupAttr,

    -- * Auxiliary
    All,
    Substructure,
    Attribute,
  )
where

import Control.Monad.Writer.CPS
import Control.MultiWalk.HasSub (All, GSubTag, HasSub (..), SubSpec (..))
import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import Ondim.MultiWalk.Combinators
import Ondim.MultiWalk.Core
import Prelude hiding (All)

-- | Runs the Ondim action with a given initial state.
evalOndimTWith ::
  Monad m =>
  OndimState tag m ->
  Ondim tag m a ->
  m (Either OndimException a)
evalOndimTWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialGS
      `evalStateT` s

runOndimTWith ::
  Monad m =>
  OndimState tag m ->
  Ondim tag m a ->
  m (Either OndimException (a, OndimState tag m))
runOndimTWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialGS
      `runStateT` s

-- | Runs the Ondim action with empty initial state.
evalOndimT :: Monad m => Ondim tag m a -> m (Either OndimException a)
evalOndimT = evalOndimTWith mempty

-- State manipulation

getOndimS :: Monad m => Ondim tag m (OndimState tag m)
getOndimS = Ondim get

modifyOndimS :: Monad m => (OndimState tag m -> OndimState tag m) -> Ondim tag m ()
modifyOndimS = Ondim . modify'

putOndimS :: Monad m => OndimState tag m -> Ondim tag m ()
putOndimS = Ondim . put

withExpansion ::
  Monad m =>
  Text ->
  Maybe (SomeExpansion tag m) ->
  Ondim tag m a ->
  Ondim tag m a
withExpansion name ex st = do
  pEx <- Ondim $ gets (lookupExpansion name . expansions)
  Ondim $ modify' \s -> s {expansions = insOrDel ex (expansions s)}
  st <* modifyOndimS \s -> s {expansions = insOrDel pEx (expansions s)}
  where
    insOrDel = maybe (deleteExpansion name) (insertExpansion name)

withFilter ::
  Monad m =>
  Text ->
  Maybe (SomeFilter tag m) ->
  Ondim tag m a ->
  Ondim tag m a
withFilter name ex st = do
  pEx <- Ondim $ gets (Map.lookup name . filters)
  Ondim $ modify' \s -> s {filters = insOrDel ex (filters s)}
  st <* modifyOndimS \s -> s {filters = insOrDel pEx (filters s)}
  where
    insOrDel = maybe (Map.delete name) (Map.insert name)

-- | "Bind" new expansions locally.
withExpansions :: Monad m => Expansions tag m -> Ondim tag m a -> Ondim tag m a
withExpansions (Expansions exps) o = foldr (\(k, v) -> withExpansion k (Just v)) o (Map.toList exps)

-- | "Bind" filters locally.
withFilters :: Monad m => Filters tag m -> Ondim tag m a -> Ondim tag m a
withFilters filt o = foldr (\(k, v) -> withFilter k (Just v)) o (Map.toList filt)

-- | "Unbind" many expansions locally.
withoutExpansions :: Monad m => [Text] -> Ondim tag m a -> Ondim tag m a
withoutExpansions names o = foldr (`withExpansion` Nothing) o names

-- | "Unbind" many expansions locally.
withoutFilters :: Monad m => [Text] -> Ondim tag m a -> Ondim tag m a
withoutFilters names o = foldr (`withFilter` Nothing) o names

-- | Put a new expansion into the local state, modifying the scope.
putExpansion :: Monad m => Text -> SomeExpansion tag m -> Ondim tag m ()
putExpansion key ex =
  modifyOndimS \s -> s {expansions = insertExpansion key ex (expansions s)}

type ExpansionMap tag m = Writer [(Text, Maybe (SomeExpansion tag m))] ()

infixr 0 #<>

(#<>) :: Text -> m -> Writer [(Text, m)] ()
name #<> ex = tell [(name, ex)]

unbind :: Text -> Writer [(Text, Maybe m)] ()
unbind k = k #<> Nothing

infixr 0 ##

(##) :: Typeable t => Text -> Expansion tag m t -> ExpansionMap tag m
name ## ex = name #<> Just $ toSomeExpansion ex

infixr 0 #@

(#@) :: Text -> Text -> ExpansionMap tag m
name #@ ex = name #<> Just $ TextData ex

infixr 0 #*

(#*) :: Text -> GlobalExpansion tag m -> ExpansionMap tag m
name #* ex = name #<> Just $ GlobalExpansion ex

infixr 0 #.

(#.) :: Text -> ExpansionMap tag m -> ExpansionMap tag m
name #. ex =
  name #<> Just . Namespace $
    foldl' (flip $ uncurry insertExpansion) mempty (mapMaybe sequence $ execWriter ex)

type FilterMap tag m = Writer [(Text, Maybe (SomeFilter tag m))] ()

infixr 0 $#

($#) :: Typeable t => Text -> Filter tag m t -> FilterMap tag m
name $# ex = name #<> Just $ toSomeFilter ex

infixr 0 $*

($*) :: Typeable t => Text -> Filter tag m t -> FilterMap tag m
name $* ex = name #<> Just $ toSomeFilter ex

-- | Infix version of @withExpansions@ to bind using MapSyntax.
binding ::
  Monad m =>
  Ondim tag m a ->
  ExpansionMap tag m ->
  Ondim tag m a
binding o exps =
  let kvs = execWriter exps
   in foldl' (flip $ uncurry withExpansion) o kvs

-- | Infix version of @withFilters@ to bind using MapSyntax.
bindingFilters ::
  Monad m =>
  Ondim tag m a ->
  FilterMap tag m ->
  Ondim tag m a
bindingFilters o filts =
  let kvs = execWriter filts
   in foldl' (flip $ uncurry withFilter) o kvs

-- Children

children ::
  forall tag t.
  ( OndimNode tag t
  ) =>
  t ->
  [t]
children = getSubstructure @t

liftChildren ::
  forall t tag m.
  ( OndimNode tag t,
    Monad m
  ) =>
  Expansion tag m t
liftChildren = liftNodes . children @tag

-- Attributes

attributes ::
  forall t tag m.
  (OndimNode tag t, Monad m) =>
  t ->
  Ondim tag m [Attribute]
attributes = liftNodes . getAttrs @tag

lookupAttr ::
  (Monad m, OndimNode tag t) =>
  Text ->
  t ->
  Ondim tag m (Maybe Text)
lookupAttr key = fmap (L.lookup key) . attributes

fromTemplate ::
  forall tag m t.
  ( OndimNode tag t,
    Monad m
  ) =>
  [t] ->
  Expansion tag m t
fromTemplate tpl inner =
  liftNodes tpl `binding` do
    "this.children" ## const (liftChildren inner)

-- | Either applies expansion 'name', or throws an error if it does not exist.
callExpansion :: forall t tag m. GlobalConstraints tag m t => Text -> Expansion tag m t
callExpansion name arg = do
  exps <- getExpansion name
  maybe (throwNotBound name) ($ arg) exps

-- | Either applies expansion 'name', or throws an error if it does not exist.
callText :: forall tag m. Monad m => Text -> Ondim tag m Text
callText name = do
  exps <- getTextData name
  maybe (throwNotBound name) pure exps
