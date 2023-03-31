{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Core
  ( Ondim (..),
    OndimTag (..),
    OndimNode (..),
    liftNode,
    liftNodes,
    liftSubstructures,
    OndimState (..),
    initialGS,
    OndimException (..),
    throwNotBound,
    throwCustom,
    GlobalConstraints,
    Expansion,
    GlobalExpansion,
    SomeExpansion (..),
    toSomeExpansion,
    splitExpansionKey,
    lookupExpansion,
    insertExpansion,
    deleteExpansion,
    Expansions (..),
    getExpansion,
    getTextData,
    Filter,
    GlobalFilter,
    SomeFilter (..),
    toSomeFilter,
    Filters,
    CanLift (..),
    modSubLift,
    Substructure (..),
    getSubstructure,
    getSubstructure',
    modSubstructure,
    modSubstructureM,
    modSubstructureM',
    Attribute,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.MultiWalk.HasSub
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Type.Reflection (TypeRep, eqTypeRep, typeRep, type (:~~:) (HRefl))
import Prelude hiding (All)

-- * Classes

class (All (OndimNode tag) (OndimTypes tag)) => OndimTag tag where
  type OndimTypes tag :: [Type]

class
  ( HasSub GSubTag (ExpTypes t) t,
    All (CanLift tag) (ExpTypes t),
    All (Substructure t) (ExpTypes t),
    Typeable t
  ) =>
  OndimNode tag t
  where
  type ExpTypes t :: [SubSpec]
  identify :: t -> Maybe Text
  identify _ = Nothing
  fromText :: Maybe (Text -> [t])
  fromText = Nothing
  getAttrs :: t -> [Attribute]
  default getAttrs ::
    ( OndimNode tag t,
      All (Substructure Attribute) (ExpTypes t)
    ) =>
    t ->
    [Attribute]
  getAttrs = getSubstructure @Attribute

-- * Attributes

-- | Alias for attributes
type Attribute = (Text, Text)

instance OndimNode tag Text where
  type ExpTypes Text = '[]

instance OndimNode tag Attribute where
  -- Manually defined instance for convenience
  type ExpTypes Attribute = '[ 'SubSpec Text Text]
  identify = Just . fst

instance CanLift tag ('SubSpec Text Text) where
  liftSub (x :: a) = mconcat <$> liftSub @tag @('SubSpec [a] a) [x]

-- * Monad

newtype Ondim tag m a = Ondim
  { unOndimT ::
      ReaderT OndimGS (StateT (OndimState tag m) (ExceptT OndimException m)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError OndimException)

instance MonadTrans (Ondim tag) where
  lift = Ondim . lift . lift . lift

instance MonadState s m => MonadState s (Ondim tag m) where
  get = lift get
  put x = lift (put x)

-- * State data

type GlobalConstraints tag m t = (OndimNode tag t, OndimTag tag, Monad m)

type Filter tag m t = t -> Ondim tag m [t] -> Ondim tag m [t]

type GlobalFilter tag m = forall a. GlobalConstraints tag m a => Filter tag m a

data SomeFilter tag m where
  SomeFilter :: TypeRep a -> Filter tag m a -> SomeFilter tag m
  GlobalFilter :: GlobalFilter tag m -> SomeFilter tag m

toSomeFilter :: Typeable a => Filter tag m a -> SomeFilter tag m
toSomeFilter = SomeFilter typeRep

getSomeFilter :: forall a tag m. GlobalConstraints tag m a => SomeFilter tag m -> Maybe (Filter tag m a)
getSomeFilter (GlobalFilter v) = Just v
getSomeFilter (SomeFilter t v)
  | Just HRefl <- t `eqTypeRep` rep = Just v
  | otherwise = Nothing
  where
    rep = typeRep :: TypeRep a

type Filters tag m = HashMap Text (SomeFilter tag m)

type Expansion tag m t = t -> Ondim tag m [t]

type GlobalExpansion tag m = forall a. GlobalConstraints tag m a => Expansion tag m a

data SomeExpansion tag m where
  SomeExpansion :: TypeRep a -> Expansion tag m a -> SomeExpansion tag m
  GlobalExpansion :: GlobalExpansion tag m -> SomeExpansion tag m
  TextData :: Text -> SomeExpansion tag m
  Namespace :: Expansions tag m -> SomeExpansion tag m

toSomeExpansion :: Typeable a => Expansion tag m a -> SomeExpansion tag m
toSomeExpansion = SomeExpansion typeRep

getSomeExpansion ::
  forall a tag m.
  GlobalConstraints tag m a =>
  SomeExpansion tag m ->
  Maybe (Expansion tag m a)
getSomeExpansion (TextData t)
  | Just f <- fromText @tag = Just (const $ pure $ f t)
  | otherwise = Nothing
getSomeExpansion (GlobalExpansion e) = Just e
getSomeExpansion (SomeExpansion t v)
  | Just HRefl <- t `eqTypeRep` typeRep @a = Just v
  | otherwise = Nothing
getSomeExpansion Namespace {} = Nothing

newtype Expansions tag m = Expansions {getExpansions :: HashMap Text (SomeExpansion tag m)}

instance Semigroup (Expansions tag m) where
  (Expansions x) <> (Expansions y) = Expansions $ Map.unionWith f x y
    where
      f (Namespace n) (Namespace m) = Namespace $ n <> m
      f z _ = z

instance Monoid (Expansions tag m) where
  mempty = Expansions mempty

splitExpansionKey :: Text -> [Text]
splitExpansionKey = T.split (\c -> c == '.' || c == ':')

lookupExpansion :: Text -> Expansions tag m -> Maybe (SomeExpansion tag m)
lookupExpansion (splitExpansionKey -> keys) (Expansions e) = go keys e
  where
    go [] _ = Nothing
    go [k] m = Map.lookup k m
    go (k : ks) m = case Map.lookup k m of
      Just (Namespace (Expansions n)) -> go ks n
      _ -> Nothing

insertExpansion :: Text -> SomeExpansion tag m -> Expansions tag m -> Expansions tag m
insertExpansion (splitExpansionKey -> keys) e (Expansions es) = Expansions $ go keys es
  where
    go [] = id
    go [k] = Map.insert k e
    go (k : ks) =
      flip Map.alter k $
        Just . Namespace . Expansions . \case
          Just (Namespace (Expansions n)) -> go ks n
          _ -> go ks mempty

deleteExpansion :: Text -> Expansions tag m -> Expansions tag m
deleteExpansion (splitExpansionKey -> keys) (Expansions es) = Expansions $ go keys es
  where
    go [] = id
    go [k] = Map.delete k
    go (k : ks) = flip Map.alter k \case
      Just (Namespace (Expansions n)) -> Just $ Namespace $ Expansions $ go ks n
      _ -> Nothing

-- | Ondim's expansion state
data OndimState tag (m :: Type -> Type) = OndimState
  { -- | Named expansions
    expansions :: Expansions tag m,
    -- | Similar to expansions, but are always applied after the expansion (the
    -- purpose of the name is just to facilitate binding/unbinding).
    filters :: Filters tag m
  }
  deriving (Generic)

instance Semigroup (OndimState tag m) where
  OndimState x1 y1 <> OndimState x2 y2 = OndimState (x1 <> x2) (y1 <> y2)

instance Monoid (OndimState tag m) where
  mempty = OndimState mempty mempty

-- | Ondim's global state
data OndimGS = OndimGS
  { expansionDepth :: Int,
    expansionTrace :: [Text]
  }
  deriving (Read, Show, Generic)

initialGS :: OndimGS
initialGS = OndimGS 0 []

-- * Exceptions

data OndimException
  = MaxExpansionDepthExceeded [Text]
  | ExpansionNotBound Text [Text]
  | CustomException Text [Text]
  deriving (Show)

throwNotBound ::
  Monad m =>
  Text ->
  Ondim tag m s
throwNotBound name =
  throwError . ExpansionNotBound name
    =<< Ondim (asks expansionTrace)

throwCustom ::
  Monad m =>
  Text ->
  Ondim tag m s
throwCustom name =
  throwError . CustomException name
    =<< Ondim (asks expansionTrace)

-- * Lifiting

getTextData :: Monad m => Text -> Ondim tag m (Maybe Text)
getTextData name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  return do
    TextData text <- mbValue
    return text

getExpansion ::
  forall t tag m.
  GlobalConstraints tag m t =>
  Text ->
  Ondim tag m (Maybe (Expansion tag m t))
getExpansion name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  return do
    expansion <- getSomeExpansion =<< mbValue
    Just (expCtx name . expansion)
{-# INLINEABLE getExpansion #-}

{- | This function recursively lifts the nodes into an unvaluated state, that will
   be evaluated with the defined expansions.
-}
liftNode ::
  forall tag m t.
  (Monad m, OndimTag tag, OndimNode tag t) =>
  t ->
  Ondim tag m [t]
liftNode node = do
  apFilters <- Ondim $ gets $ foldr (\f g -> f node . g) id . mapMaybe (getSomeFilter @t) . Map.elems . filters
  apFilters $
    case identify @tag node of
      Just name -> expand name
      _ -> one <$> liftSubstructures node
  where
    expand name =
      getExpansion name >>= \case
        Just expansion -> expansion node
        Nothing -> withDebugCtx id (name :) $ one <$> liftSubstructures node
{-# INLINEABLE liftNode #-}

-- | Lift a list of nodes, applying filters.
liftNodes ::
  forall tag m t.
  (Monad m, OndimNode tag t, OndimTag tag) =>
  [t] ->
  Ondim tag m [t]
liftNodes = foldMapM (liftNode @tag)

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
withDebugCtx f g =
  Ondim
    . local
      ( \gs ->
          gs
            { expansionDepth = f (expansionDepth gs),
              expansionTrace = g (expansionTrace gs)
            }
      )
    . unOndimT

expCtx :: forall tag m a. Monad m => Text -> Ondim tag m a -> Ondim tag m a
expCtx name ctx = do
  gst <- Ondim ask
  if expansionDepth gst >= 200
    then -- To avoid recursive expansions
      throwError (MaxExpansionDepthExceeded $ expansionTrace gst)
    else withDebugCtx (+ 1) (name :) ctx
