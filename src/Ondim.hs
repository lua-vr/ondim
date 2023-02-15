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
    evalOndimTWith,
    evalOndimT,

    -- * Exceptions
    OndimException (..),
    throwNotBound,
    throwCustom,

    -- * State transformations

    -- Expansions
    Expansions',
    binding,
    unbinding,
    withExpansions,
    withoutExpansion,
    withoutExpansions,
    filteringExpansions,
    putExpansion,
    getExpansion,
    callExpansion,
    -- Filters
    Filters',
    bindingFilters,
    unbindingFilters,
    withFilters,
    withoutFilter,
    withoutFilters,
    -- Text
    bindingText,
    unbindingText,
    withText,
    withoutText,
    withoutTexts,
    putTextExpansion,
    getTextExpansion,
    callText,
    -- State manipulation
    getOndimMS,
    putOndimMS,
    withOndimGS,
    withOndimS,

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
    ContainsState (..),

    -- * MapSyntax
    MapSyntax,
    MapSyntaxM,
    (##),
  )
where

import Control.Monad.Trans.MultiState.Strict (MultiStateT (..), runMultiStateTA, runMultiStateTAS)
import Control.MultiWalk.HasSub (All, GSubTag, HasSub (..), SubSpec (..))
import Data.HList.ContainsType (getHListElem, setHListElem)
import Data.HList.HList (HList (..))
import Data.HashMap.Strict qualified as Map
import Data.Map.Syntax (MapSyntax, MapSyntaxM, runMapSyntax', (##))
import Ondim.MultiState (mGets, mModify)
import Ondim.MultiWalk.Combinators
import Ondim.MultiWalk.Core
import Relude.Extra.Lens
import Relude.Extra.Map (delete, insert, keys, lookup)
import Prelude hiding (All)

-- | Initial global state
initialOGS :: OndimGS tag m
initialOGS = OndimGS 0 [] mempty

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
evalOndimTWith ::
  forall tag m a.
  Monad m =>
  OndimMS tag m ->
  Ondim tag m a ->
  m (Either OndimException a)
evalOndimTWith (OndimMS s) o =
  runExceptT $
    runMultiStateTA s (unOndimT o)

runOndimTWith ::
  forall tag m a.
  Monad m =>
  OndimMS tag m ->
  Ondim tag m a ->
  m (Either OndimException (a, OndimMS tag m))
runOndimTWith (OndimMS s) o =
  runExceptT $
    second OndimMS
      <$> runMultiStateTAS s (unOndimT o)

-- | Runs the Ondim action with empty initial state.
evalOndimT ::
  forall tag m a.
  ( HasInitialMultiState (OndimTypes tag),
    Monad m
  ) =>
  Ondim tag m a ->
  m (Either OndimException a)
evalOndimT = evalOndimTWith initialMS

-- State manipulation

getOndimMS :: Monad m => Ondim tag m (OndimMS tag m)
getOndimMS = OndimMS <$> Ondim (MultiStateT get)

putOndimMS :: Monad m => OndimMS tag m -> Ondim tag m ()
putOndimMS (OndimMS s) = Ondim (MultiStateT (put s))

withOndimGS ::
  Monad m =>
  (OndimGS tag m -> OndimGS tag m) ->
  Ondim tag m a ->
  Ondim tag m a
withOndimGS f st =
  Ondim $ MultiStateT $ StateT $ \s@(os :+: ms) ->
    (,s) <$> runMultiStateTA (f os :+: ms) (unOndimT st)
{-# INLINEABLE withOndimGS #-}

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
withText :: Monad m => HashMap Text (Ondim tag m Text) -> Ondim tag m a -> Ondim tag m a
withText exps = withOndimGS (\s -> s {textExpansions = exps <> textExpansions s})

-- | "Unbind" an expansion locally.
withoutExpansion :: forall t tag m a. (OndimNode tag t, Monad m) => Text -> Ondim tag m a -> Ondim tag m a
withoutExpansion name = withOndimS @t (\s -> s {expansions = delete name (expansions s)})

-- | "Unbind" many expansions locally.
withoutExpansions :: forall t tag m a. (OndimNode tag t, Monad m) => [Text] -> Ondim tag m a -> Ondim tag m a
withoutExpansions names = withOndimS @t (\s -> s {expansions = foldr delete (expansions s) names})

-- | Filter bound expansions by name.
filteringExpansions :: forall t tag m a. (OndimNode tag t, Monad m) => (Text -> Bool) -> Ondim tag m a -> Ondim tag m a
filteringExpansions p = withOndimS @t (\s -> s {expansions = Map.filterWithKey (const . p) (expansions s)})

-- | "Unbind" a filter locally.
withoutFilter :: forall t tag m a. (OndimNode tag t, Monad m) => Text -> Ondim tag m a -> Ondim tag m a
withoutFilter name = withOndimS @t (\s -> s {filters = delete name (filters s)})

-- | "Unbind" many filters locally.
withoutFilters :: forall t tag m a. (OndimNode tag t, Monad m) => [Text] -> Ondim tag m a -> Ondim tag m a
withoutFilters names = withOndimS @t (\s -> s {filters = foldr delete (filters s) names})

-- | "Unbind" a text expansion locally.
withoutText :: Monad m => Text -> Ondim tag m a -> Ondim tag m a
withoutText name = withOndimGS (\s -> s {textExpansions = delete name (textExpansions s)})

-- | "Unbind" many text expansions locally.
withoutTexts :: Monad m => [Text] -> Ondim tag m a -> Ondim tag m a
withoutTexts names = withOndimGS (\s -> s {textExpansions = foldr delete (textExpansions s) names})

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

runMapNoErrors :: (Eq k, Hashable k) => MapSyntaxM k v a -> HashMap k v
runMapNoErrors =
  fromRight mempty
    . runMapSyntax' (\_ new _ -> Just new) Map.lookup Map.insert

-- | Infix version of @withExpansions@ to bind using MapSyntax.
binding ::
  (OndimNode tag t, Monad m) =>
  Ondim tag m a ->
  Expansions' tag m t ->
  Ondim tag m a
binding o exps = withExpansions (runMapNoErrors exps) o

-- | Infix version of @withFilters@ to bind using MapSyntax.
bindingFilters ::
  (OndimNode tag t, Monad m) =>
  Ondim tag m a ->
  Filters' tag m t ->
  Ondim tag m a
bindingFilters o filts = withFilters (runMapNoErrors filts) o

-- | Infix version of @withText@ to bind using MapSyntax.
bindingText ::
  Monad m =>
  Ondim tag m a ->
  MapSyntax Text (Ondim tag m Text) ->
  Ondim tag m a
bindingText o exps = withText (runMapNoErrors exps) o

-- | Infix version of @withoutExpansions@ to unbind many expansions locally.
unbinding :: forall t tag m a. (OndimNode tag t, Monad m) => Ondim tag m a -> [Text] -> Ondim tag m a
unbinding = flip (withoutExpansions @t)

-- | Infix version of @withoutFilters@ to unbind many filters locally.
unbindingFilters :: forall t tag m a. (OndimNode tag t, Monad m) => Ondim tag m a -> [Text] -> Ondim tag m a
unbindingFilters = flip (withoutFilters @t)

-- | Infix version of @withoutTexts@ to unbind many text expansions locally.
unbindingText :: Monad m => Ondim tag m a -> [Text] -> Ondim tag m a
unbindingText = flip withoutTexts

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
callExpansion :: forall t tag m. (OndimNode tag t, Monad m) => Text -> Expansion tag m t
callExpansion name arg = do
  exps <- getExpansion name
  maybe (throwNotBound @t name) ($ arg) exps

callText ::
  Monad m =>
  Text ->
  Ondim tag m Text
callText k = fromMaybe (throwNotBound @Text k) =<< getTextExpansion k
