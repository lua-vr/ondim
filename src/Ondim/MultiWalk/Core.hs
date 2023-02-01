{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Core where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.MultiState.Strict (MultiStateT (..), mGet, runMultiStateT)
import Control.MultiWalk.HasSub
import Data.HList.ContainsType (ContainsType (..))
import Data.HList.HList (HList (..))
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Typeable (TypeRep, typeRep)
import Ondim.MultiState (mGets)
import Prelude hiding (All)

-- * Classes

class (All (OndimNode tag) (OndimTypes tag)) => OndimTag tag where
  type OndimTypes tag :: [Type]

class
  ( HasSub GSubTag (ExpTypes t) t,
    ContainsState (OndimTypes tag) t,
    All (CanLift tag) (ExpTypes t),
    All (Substructure t) (ExpTypes t),
    Typeable t
  ) =>
  OndimNode tag t
  where
  type ExpTypes t :: [SubSpec]
  identify :: t -> Maybe Text
  identify _ = Nothing
  rename :: Text -> t -> t
  rename _ = id
  fromText :: Maybe (Text -> [t])
  fromText = Nothing
  validIdentifiers :: Maybe [Text]
  validIdentifiers = Nothing

-- * Monad

newtype Ondim tag m a = Ondim
  { unOndimT ::
      MultiStateT
        (OndimGS tag m : MultiOndimS tag m)
        (ExceptT OndimException m)
        a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState s)

instance MonadTrans (Ondim tag) where
  lift = Ondim . lift . lift

-- * State data

type Filter tag m t = Ondim tag m [t] -> Ondim tag m [t]

type Filters tag m t = HashMap Text (Filter tag m t)

type Expansion tag (m :: Type -> Type) (t :: Type) = t -> Ondim tag m [t]

type Expansions tag (m :: Type -> Type) (t :: Type) = HashMap Text (Expansion tag m t)

-- | Ondim's state (one for each type)
data OndimS tag (m :: Type -> Type) (t :: Type) = OndimS
  { -- | Named expansions
    expansions :: Expansions tag m t,
    -- | Similar to expansions, but are always applied after the expansion (the
    -- purpose of the name is just to facilitate binding/unbinding).
    filters :: Filters tag m t
  }
  deriving (Generic)

instance Semigroup (OndimS tag m t) where
  OndimS x1 y1 <> OndimS x2 y2 = OndimS (x1 <> x2) (y1 <> y2)

instance Monoid (OndimS tag m t) where
  mempty = OndimS mempty mempty

-- | Ondim's global state
data OndimGS tag (m :: Type -> Type) = OndimGS
  { expansionDepth :: Int,
    expansionTrace :: [Text],
    textExpansions :: HashMap Text (Ondim tag m Text)
  }
  deriving (Generic)

type family MultiOndimS' tag (m :: Type -> Type) (l :: [Type]) where
  MultiOndimS' _ _ '[] = '[]
  MultiOndimS' tag m (l : ls) = OndimS tag m l : MultiOndimS' tag m ls

-- | A list of types with all the OndimS types associated to the tag.
type MultiOndimS tag m = MultiOndimS' tag m (OndimTypes tag)

-- * Exceptions

data OndimException
  = MaxExpansionDepthExceeded [Text]
  | ExpansionNotBound Text TypeRep [Text]
  | CustomException Text [Text]
  deriving (Show)

instance Monad m => MonadError OndimException (Ondim tag m) where
  throwError = Ondim . MultiStateT . throwError
  catchError m h = to' $ catchError (from m) (from . h)
    where
      to' = Ondim . MultiStateT
      from = runMultiStateTRaw . unOndimT

throwNotBound ::
  forall t tag m s.
  (Typeable t, Monad m) =>
  Text ->
  Ondim tag m s
throwNotBound name =
  throwError . ExpansionNotBound name (typeRep (Proxy @t))
    =<< Ondim (mGets @(OndimGS tag m) expansionTrace)

throwCustom ::
  forall tag m s.
  (Monad m) =>
  Text ->
  Ondim tag m s
throwCustom name =
  throwError . CustomException name
    =<< Ondim (mGets @(OndimGS tag m) expansionTrace)

-- * Lifiting

getExpansion ::
  forall t tag m.
  (Monad m, OndimNode tag t) =>
  Text ->
  Ondim tag m (Maybe (Expansion tag m t))
getExpansion name = do
  gst <- Ondim $ mGet @(OndimGS tag m)
  st <- stGet
  if
      | Just fT <- fromText @tag,
        Just text <- Map.lookup name (textExpansions gst) ->
          pure $ Just (const $ expCtx name $ fT <$> text)
      | Just expansion <- Map.lookup name (expansions st) ->
          pure $ Just (expCtx name . expansion)
      | otherwise -> pure Nothing
{-# INLINEABLE getExpansion #-}

-- | This function recursively lifts the nodes into an unvaluated state, that will
--   be evaluated with the defined expansions.
liftNode ::
  forall tag m t.
  (Monad m, OndimTag tag, OndimNode tag t) =>
  t ->
  Ondim tag m [t]
liftNode node =
  case identify @tag node of
    Just name ->
      case T.stripSuffix "_" name of
        Just name' -> do
          let node' = rename @tag name' node
          expand name' node' `catchError` \case
            ExpansionNotBound {} -> pure []
            x -> throwError x
        Nothing -> expand name node
    _ -> pure <$> liftSubstructures node
  where
    expand name node' =
      getExpansion name >>= \case
        Just expansion -> expansion node'
        Nothing
          | Just valid <- validIdentifiers @tag @t,
            name `notElem` valid ->
              throwNotBound @t name
          | otherwise -> pure <$> liftSubstructures node'
{-# INLINEABLE liftNode #-}

-- | Lift a list of nodes, applying filters.
liftNodes ::
  forall tag m t.
  (Monad m, OndimNode tag t, OndimTag tag) =>
  [t] ->
  Ondim tag m [t]
liftNodes nodes = do
  st <- stGet
  foldr ($) (foldMapM (liftNode @tag) nodes) (filters st)

modSubLift ::
  forall tag ls m t.
  ( Monad m,
    OndimTag tag,
    HasSub GSubTag ls t,
    All (CanLift tag) ls
  ) =>
  t ->
  Ondim tag m t
modSubLift = modSub @GSubTag @ls @t (Proxy @(CanLift tag)) (\(_ :: Proxy s) -> liftSub @tag @s)
{-# INLINEABLE modSubLift #-}

getSubstructure' ::
  forall a ls t.
  ( HasSub GSubTag ls t,
    All (Substructure a) ls
  ) =>
  t ->
  [a]
getSubstructure' = getSub @GSubTag @ls @t (Proxy @(Substructure a)) (\(_ :: Proxy j) -> getSubs @a @j)

getSubstructure ::
  forall a t.
  ( HasSub GSubTag (ExpTypes t) t,
    All (Substructure a) (ExpTypes t)
  ) =>
  t ->
  [a]
getSubstructure = getSubstructure' @a @(ExpTypes t)

modSubstructureM' ::
  forall a ls t m.
  ( HasSub GSubTag ls t,
    All (Substructure a) ls,
    Applicative m
  ) =>
  ([a] -> m [a]) ->
  t ->
  m t
modSubstructureM' f = modSub @GSubTag @ls @t (Proxy @(Substructure a)) (\(_ :: Proxy j) -> modSubs @a @j f)

modSubstructureM ::
  forall a t m.
  ( HasSub GSubTag (ExpTypes t) t,
    All (Substructure a) (ExpTypes t),
    Applicative m
  ) =>
  ([a] -> m [a]) ->
  t ->
  m t
modSubstructureM = modSubstructureM' @a @(ExpTypes t)

modSubstructure ::
  forall a t.
  ( HasSub GSubTag (ExpTypes t) t,
    All (Substructure a) (ExpTypes t)
  ) =>
  ([a] -> [a]) ->
  t ->
  t
modSubstructure f = runIdentity . modSubstructureM @a (Identity . f)

-- | Lift only the substructures of a node.
liftSubstructures :: forall tag m t. (Monad m, OndimTag tag, OndimNode tag t) => t -> Ondim tag m t
liftSubstructures = modSubLift @tag @(ExpTypes t)
{-# INLINEABLE liftSubstructures #-}

class CanLift (tag :: Type) (s :: SubSpec) where
  liftSub ::
    (OndimTag tag, Monad m) =>
    SpecCarrier s ->
    Ondim tag m (SpecCarrier s)

class Substructure (a :: Type) (s :: SubSpec) where
  getSubs :: SpecCarrier s -> [a]
  modSubs :: Applicative m => ([a] -> m [a]) -> SpecCarrier s -> m (SpecCarrier s)

instance OndimNode tag a => CanLift tag ('SubSpec [a] a) where
  liftSub = liftNodes

instance {-# OVERLAPPABLE #-} Substructure a s where
  getSubs = mempty
  modSubs = const pure

instance Substructure a ('SubSpec [a] a) where
  getSubs = id
  modSubs = id

-- * Expansion context

withDebugCtx ::
  forall tag m a.
  Monad m =>
  (Int -> Int) ->
  ([Text] -> [Text]) ->
  Ondim tag m a ->
  Ondim tag m a
withDebugCtx f g (Ondim comp) =
  Ondim $ MultiStateT $ StateT $ \s -> do
    let gs :: OndimGS tag m = getHListElem s
        depth' = expansionDepth gs
        trace' = expansionTrace gs
        gs' = gs {expansionDepth = f depth', expansionTrace = g trace'}
        s' = setHListElem gs' s
    (out, s'') <- runMultiStateT s' comp
    let gs'' :: OndimGS tag m = getHListElem s''
        gs''' = gs'' {expansionDepth = depth', expansionTrace = trace'}
        s''' = setHListElem gs''' s''
    pure (out, s''')

expCtx :: forall tag m a. Monad m => Text -> Ondim tag m a -> Ondim tag m a
expCtx name ctx = do
  gst <- Ondim $ mGet @(OndimGS tag m)
  if expansionDepth gst >= 200
    then -- To avoid recursive expansions
      throwError (MaxExpansionDepthExceeded $ expansionTrace gst)
    else withDebugCtx (+ 1) (name :) ctx

-- * Well... GHC is not that smart for some things

class ContainsState (ls :: [Type]) l where
  getState :: HList (MultiOndimS' tag m ls) -> OndimS tag m l
  setState :: OndimS tag m l -> HList (MultiOndimS' tag m ls) -> HList (MultiOndimS' tag m ls)

instance {-# OVERLAPPING #-} ContainsState (l : ls) l where
  getState (x :+: _) = x
  setState x (_ :+: xs) = x :+: xs

instance ContainsState ls l => ContainsState (s : ls) l where
  getState (_ :+: xs) = getState @ls xs
  setState y (x :+: xs) = x :+: setState @ls y xs

stGet :: forall t tag m. (OndimNode tag t, Monad m) => Ondim tag m (OndimS tag m t)
stGet =
  Ondim $ MultiStateT $ StateT $ \s@(_ :+: ls) ->
    pure (getState @(OndimTypes tag) ls, s)
{-# INLINE stGet #-}

stSet :: forall t tag m. (OndimNode tag t, Monad m) => OndimS tag m t -> Ondim tag m ()
stSet st =
  Ondim $ MultiStateT $ StateT $ \(gs :+: ls) ->
    pure ((), gs :+: setState @(OndimTypes tag) st ls)
{-# INLINE stSet #-}

stState :: (OndimNode tag s, Monad m) => (OndimS tag m s -> (a, OndimS tag m s)) -> Ondim tag m a
stState f = do
  s <- stGet
  let ~(a, s') = f s
  stSet s'
  return a
{-# INLINE stState #-}

stModify :: (Monad m, OndimNode tag s) => (OndimS tag m s -> OndimS tag m s) -> Ondim tag m ()
stModify f = stState (\s -> ((), f s))
{-# INLINE stModify #-}

stGets :: (Monad m, OndimNode tag s) => (OndimS tag m s -> b) -> Ondim tag m b
stGets f = f <$> stGet
{-# INLINE stGets #-}
