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
    OndimTag (..),
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
    Expansion,
    Expansions,
    Filter,
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
    ExpansionMap,
    (##),
    (#@),
    (#*),
    binding,
    unbinding,
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
    unbindingFilters,
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

    -- * Auxiliary
    All,
    Substructure,
    Attribute,
  )
where

import Control.Monad.Writer.CPS
import Control.MultiWalk.HasSub (All, GSubTag, HasSub (..), SubSpec (..))
import Data.HashMap.Strict qualified as Map
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

bindingLocally ::
  Monad m =>
  Text ->
  Maybe (SomeExpansion tag m) ->
  Ondim tag m a ->
  Ondim tag m a
bindingLocally name ex st = do
  pEx <- Ondim $ gets (Map.lookup name . expansions)
  Ondim $ modify' \s -> s {expansions = insOrDel ex (expansions s)}
  st <* modifyOndimS \s -> s {expansions = insOrDel pEx (expansions s)}
  where
    insOrDel = maybe (Map.delete name) (Map.insert name)

bindingFilterLocally ::
  Monad m =>
  Text ->
  Maybe (SomeFilter tag m) ->
  Ondim tag m a ->
  Ondim tag m a
bindingFilterLocally name ex st = do
  pEx <- Ondim $ gets (Map.lookup name . filters)
  Ondim $ modify' \s -> s {filters = insOrDel ex (filters s)}
  st <* modifyOndimS \s -> s {filters = insOrDel pEx (filters s)}
  where
    insOrDel = maybe (Map.delete name) (Map.insert name)

-- | "Bind" new expansions locally.
withExpansions :: Monad m => Expansions tag m -> Ondim tag m a -> Ondim tag m a
withExpansions exps o = foldr (\(k, v) -> bindingLocally k (Just v)) o (Map.toList exps)

-- | "Bind" filters locally.
withFilters :: Monad m => Filters tag m -> Ondim tag m a -> Ondim tag m a
withFilters filt o = foldr (\(k, v) -> bindingFilterLocally k (Just v)) o (Map.toList filt)

-- | "Unbind" many expansions locally.
withoutExpansions :: Monad m => [Text] -> Ondim tag m a -> Ondim tag m a
withoutExpansions names o = foldr (`bindingLocally` Nothing) o names

-- | "Unbind" many expansions locally.
withoutFilters :: Monad m => [Text] -> Ondim tag m a -> Ondim tag m a
withoutFilters names o = foldr (`bindingFilterLocally` Nothing) o names

-- | Put a new expansion into the local state, modifying the scope.
putExpansion :: Monad m => Text -> SomeExpansion tag m -> Ondim tag m ()
putExpansion key ex =
  modifyOndimS \s -> s {expansions = Map.insert key ex (expansions s)}

type ExpansionMap tag m = Writer (HashMap Text (SomeExpansion tag m)) ()

infixr 0 #<>

(#<>) :: Text -> m -> Writer (HashMap Text m) ()
name #<> ex = tell $ Map.singleton name ex

infixr 0 ##

(##) :: Typeable t => Text -> Expansion tag m t -> ExpansionMap tag m
name ## ex = name #<> toSomeExpansion ex

infixr 0 #@

(#@) :: Text -> Text -> ExpansionMap tag m
name #@ ex = name #<> TextData ex

infixr 0 #*

(#*) :: Text -> GlobalExpansion tag m -> ExpansionMap tag m
name #* ex = name #<> GlobalExpansion ex

type FilterMap tag m = Writer (HashMap Text (SomeFilter tag m)) ()

infixr 0 $#

($#) :: Typeable t => Text -> Filter tag m t -> FilterMap tag m
name $# ex = name #<> toSomeFilter ex

infixr 0 $*

($*) :: Typeable t => Text -> Filter tag m t -> FilterMap tag m
name $* ex = name #<> toSomeFilter ex

-- | Infix version of @withExpansions@ to bind using MapSyntax.
binding ::
  Monad m =>
  Ondim tag m a ->
  ExpansionMap tag m ->
  Ondim tag m a
binding o exps = withExpansions (execWriter exps) o

-- | Infix version of @withFilters@ to bind using MapSyntax.
bindingFilters ::
  Monad m =>
  Ondim tag m a ->
  FilterMap tag m ->
  Ondim tag m a
bindingFilters o filts = withFilters (execWriter filts) o

-- | Infix version of @withoutExpansions@ to unbind many expansions locally.
unbinding :: Monad m => Ondim tag m a -> [Text] -> Ondim tag m a
unbinding = flip withoutExpansions

-- | Infix version of @withoutFilters@ to unbind many filters locally.
unbindingFilters :: Monad m => Ondim tag m a -> [Text] -> Ondim tag m a
unbindingFilters = flip withoutFilters

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
    OndimTag tag,
    Monad m
  ) =>
  Expansion tag m t
liftChildren = liftNodes . children @tag

fromTemplate ::
  forall tag m t.
  ( OndimNode tag t,
    Monad m,
    OndimTag tag
  ) =>
  Text ->
  [t] ->
  Expansion tag m t
fromTemplate name tpl inner =
  liftNodes tpl `binding` do
    name <> ":content" ## const (liftChildren inner)

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
