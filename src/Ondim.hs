{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
    OndimGS (..),
    OndimS (..),
    OndimMS,
    -- Initial states
    initialOGS,
    initialOS,
    ondimState,
    ondimGState,
    initialMS,
    HasInitialMultiState,

    -- * Monad
    Ondim,
    runOndimTWith,
    runOndimT,

    -- * Exceptions
    OndimException (..),
    throwNotBound,

    -- * State transformations

    -- Expansions
    Expansions',
    binding,
    withExpansions,
    withoutExpansion,
    putExpansion,
    getExpansion,
    callExpansion,
    -- Filters
    Filters',
    bindingFilters,
    withFilters,
    withoutFilter,
    -- Text
    bindingText,
    withText,
    withoutText,
    putTextExpansion,
    getTextExpansion,
    callText,
    -- Global state
    withOndimGS,
    withOndimS,
    inhibitingExpansions,

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
    children',
    children,

    -- * Auxiliary
    All,
    Substructure,
    ContainsState (..),
  )
where

import Control.Monad.Trans.MultiState.Strict (MultiStateT (..), runMultiStateTA)
import Control.MultiWalk.HasSub (All, GSubTag, HasSub (..), SubSpec (..))
import Data.HList.ContainsType (getHListElem, setHListElem)
import Data.HList.HList (HList (..))
import Data.Map.Syntax (MapSyntax, runMap, (##))
import Ondim.MultiState (mGets, mModify)
import Ondim.MultiWalk.Combinators
import Ondim.MultiWalk.Core
import Relude.Extra.Lens
import Relude.Extra.Map (delete, insert, keys, lookup)
import Prelude hiding (All)

-- | Initial global state
initialOGS :: OndimGS tag m
initialOGS = OndimGS 0 [] False mempty

-- | Initial state
initialOS :: OndimS tag m t
initialOS = OndimS mempty mempty

ondimState :: forall tag m t. OndimNode tag t => Lens' (OndimMS tag m) (OndimS tag m t)
ondimState = lens getMS setMS
  where
    setMS (OndimMS (gs :+: ms)) s = OndimMS $ gs :+: setState @(OndimTypes tag) s ms
    getMS (OndimMS (_ :+: ms)) = getState @(OndimTypes tag) ms

ondimGState :: forall tag m. Lens' (OndimMS tag m) (OndimGS tag m)
ondimGState = lens getGS setGS
  where
    setGS (OndimMS ms) s = OndimMS $ setHListElem s ms
    getGS (OndimMS ms) = getHListElem ms

-- | Newtype joining OndimS and OndimGS together into a "multistate".
newtype OndimMS tag m = OndimMS (HList (OndimGS tag m : MultiOndimS tag m))
  deriving (Generic)

-- Initial state

class HasInitialMultiState (ls :: [Type]) where
  initialOMS :: HList (MultiOndimS' tag m ls)

instance HasInitialMultiState '[] where
  initialOMS = HNil

instance HasInitialMultiState ls => HasInitialMultiState (l : ls) where
  initialOMS :: forall tag m. HList (MultiOndimS' tag m (l : ls))
  initialOMS = initialOS :+: initialOMS @ls @tag @m

class ConcatHLists (ls :: [Type]) where
  concatHLists :: HList ls -> HList ls -> HList ls

instance ConcatHLists '[] where
  concatHLists _ _ = HNil

instance (ConcatHLists ls, Monoid l) => ConcatHLists (l : ls) where
  concatHLists (a :+: as) (b :+: bs) = (a <> b) :+: concatHLists as bs

instance ConcatHLists (MultiOndimS tag m) => Semigroup (OndimMS tag m) where
  OndimMS (s :+: x) <> OndimMS (_ :+: y) = OndimMS (s :+: concatHLists x y)

initialMS :: forall tag m. HasInitialMultiState (OndimTypes tag) => OndimMS tag m
initialMS = OndimMS (initialOGS :+: initialOMS @(OndimTypes tag) @tag @m)

-- | Runs the Ondim action with a given initial state.
runOndimTWith ::
  forall tag m a.
  Monad m =>
  OndimMS tag m ->
  Ondim tag m a ->
  m (Either OndimException a)
runOndimTWith (OndimMS s) o =
  runExceptT $
    runMultiStateTA s (unOndimT o)

-- | Runs the Ondim action with empty initial state.
runOndimT ::
  forall tag m a.
  ( HasInitialMultiState (OndimTypes tag),
    Monad m
  ) =>
  Ondim tag m a ->
  m (Either OndimException a)
runOndimT = runOndimTWith initialMS

withOndimGS ::
  Monad m =>
  (OndimGS tag m -> OndimGS tag m) ->
  Ondim tag m a ->
  Ondim tag m a
withOndimGS f st =
  Ondim $ MultiStateT $ StateT $ \s@(os :+: ms) ->
    (,s) <$> runMultiStateTA (f os :+: ms) (unOndimT st)
{-# INLINEABLE withOndimGS #-}

inhibitingExpansions :: Monad m => Ondim tag m a -> Ondim tag m a
inhibitingExpansions = withOndimGS (\s -> s {inhibitExpansion = True})

-- | This function works like @withReaderT@, in the sense that it creates a new
--   scope for the state in which state changes do not leak outside.
withOndimS ::
  forall t tag m a.
  (Monad m, OndimNode tag t) =>
  (OndimS tag m t -> OndimS tag m t) ->
  Ondim tag m a ->
  Ondim tag m a
withOndimS f st =
  Ondim $ MultiStateT $ StateT $ \s@(os :+: ms) ->
    (,s)
      <$> runMultiStateTA
        (os :+: setState @(OndimTypes tag) (f $ getState @(OndimTypes tag) ms) ms)
        (unOndimT st)
{-# INLINEABLE withOndimS #-}

getTextExpansion ::
  Monad m =>
  Text ->
  Ondim tag m (Maybe (Ondim tag m Text))
getTextExpansion k = expCtx k (Ondim $ mGets $ lookup k . textExpansions)

-- | "Bind" new expansions locally.
withExpansions :: (OndimNode tag t, Monad m) => Expansions tag m t -> Ondim tag m a -> Ondim tag m a
withExpansions exps =
  withOndimGS (\s -> s {textExpansions = foldr delete (textExpansions s) names})
    . withOndimS (\s -> s {expansions = exps <> expansions s})
  where
    names = keys exps

-- | "Bind" filters locally.
withFilters :: (OndimNode tag t, Monad m) => Filters tag m t -> Ondim tag m a -> Ondim tag m a
withFilters filt = withOndimS (\s -> s {filters = filt <> filters s})

-- | "Bind" text expansions locally.
withText :: Monad m => Map Text (Ondim tag m Text) -> Ondim tag m a -> Ondim tag m a
withText exps = withOndimGS (\s -> s {textExpansions = exps <> textExpansions s})

-- | "Unbind" an expansion locally.
withoutExpansion :: forall t tag m a. (OndimNode tag t, Monad m) => Text -> Ondim tag m a -> Ondim tag m a
withoutExpansion name = withOndimS @t (\s -> s {expansions = delete name (expansions s)})

-- | "Unbind" a filter locally.
withoutFilter :: forall t tag m a. (OndimNode tag t, Monad m) => Text -> Ondim tag m a -> Ondim tag m a
withoutFilter name = withOndimS @t (\s -> s {filters = delete name (filters s)})

-- | "Unbind" a text expansion locally.
withoutText :: Monad m => Text -> Ondim tag m a -> Ondim tag m a
withoutText name = withOndimGS (\s -> s {textExpansions = delete name (textExpansions s)})

-- | Put a new expansion into the local state, modifying the scope.
putExpansion :: (OndimNode tag t, Monad m) => Text -> Expansion tag m t -> Ondim tag m ()
putExpansion key exps =
  stModify (\s -> s {expansions = insert key exps (expansions s)})

-- | Put a new expansion into the local state, modifying the scope.
putTextExpansion :: Monad m => Text -> Ondim tag m Text -> Ondim tag m ()
putTextExpansion key exps =
  Ondim $ mModify (\s -> s {textExpansions = insert key exps (textExpansions s)})

type Expansions' tag m t = MapSyntax Text (Expansion tag m t)

type Filters' tag m t = MapSyntax Text (Filter tag m t)

-- | Convenience function to bind using MapSyntax.
binding ::
  (OndimNode tag t, Monad m) =>
  Ondim tag m a ->
  Expansions' tag m t ->
  Ondim tag m a
binding o exps = withExpansions (fromRight mempty (runMap exps)) o

-- | Convenience function to bind using MapSyntax.
bindingFilters ::
  (OndimNode tag t, Monad m) =>
  Ondim tag m a ->
  Filters' tag m t ->
  Ondim tag m a
bindingFilters o filts = withFilters (fromRight mempty (runMap filts)) o

-- | Convenience function to bind using MapSyntax.
bindingText ::
  Monad m =>
  Ondim tag m a ->
  MapSyntax Text (Ondim tag m Text) ->
  Ondim tag m a
bindingText o exps = withText (fromRight mempty (runMap exps)) o

children' ::
  forall tag t.
  ( OndimNode tag t
  ) =>
  t ->
  [t]
children' = getSubstructure @t

children ::
  forall t tag m.
  ( OndimNode tag t,
    Functor m
  ) =>
  Ondim tag m t ->
  Ondim tag m [t]
children = fmap (children' @tag)

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
    name <> ":content" ## const (children inner)

-- | Either applies expansion 'name', or throws an error if it does not exist.
callExpansion :: forall t tag m. (OndimNode tag t, Monad m) => Text -> Expansion tag m t
callExpansion name arg = do
  exps <- getExpansion name
  maybe (throwNotBound @t name) ($ arg) exps

callText ::
  Monad m =>
  Text ->
  Ondim tag m Text
callText k = fromMaybe (throwNotBound @Text k) =<< getTextExpansion k
