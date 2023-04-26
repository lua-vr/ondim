{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Basic where

import Control.Monad.Except (MonadError)
import Control.MultiWalk.HasSub (AllMods, GSubTag, SelSpec, SubSpec (..))
import Control.MultiWalk.HasSub qualified as HS
import Data.HashMap.Strict qualified as Map
import Type.Reflection (TypeRep)

-- * Monad

newtype Ondim m a = Ondim
  { unOndimT ::
      ReaderT OndimGS (StateT (OndimState m) (ExceptT OndimException m)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError OndimException)

instance MonadTrans Ondim where
  lift = Ondim . lift . lift . lift

instance MonadState s m => MonadState s (Ondim m) where
  get = lift get
  put x = lift (put x)

instance MonadReader s m => MonadReader s (Ondim m) where
  ask = lift ask
  local f (Ondim (ReaderT g)) = Ondim $ ReaderT $ local f . g

-- * Filters and Expansions

type GlobalConstraints m t = (OndimNode t, Monad m)

-- Filters

type Filter m t = t -> Ondim m [t] -> Ondim m [t]
type Filters m = Map Text (SomeFilter m)
type GlobalFilter m = forall a. GlobalConstraints m a => Filter m a

data SomeFilter m where
  SomeFilter :: TypeRep a -> Filter m a -> SomeFilter m
  GlobalFilter :: GlobalFilter m -> SomeFilter m

-- Expansions

type Expansion m t = t -> Ondim m [t]
newtype Expansions m = Expansions {getExpansions :: HashMap Text (SomeExpansion m)}
type GlobalExpansion m = forall a. GlobalConstraints m a => Expansion m a

data SomeExpansion m where
  SomeExpansion :: TypeRep a -> Expansion m a -> SomeExpansion m
  GlobalExpansion :: GlobalExpansion m -> SomeExpansion m
  TextData :: Text -> SomeExpansion m
  Namespace :: Expansions m -> SomeExpansion m

instance Semigroup (Expansions m) where
  (Expansions x) <> (Expansions y) = Expansions $ Map.unionWith f x y
    where
      f (Namespace n) (Namespace m) = Namespace $ n <> m
      f z _ = z

instance Monoid (Expansions m) where
  mempty = Expansions mempty

-- * State data

-- | Ondim's expansion state
data OndimState (m :: Type -> Type) = OndimState
  { -- | Named expansions
    expansions :: Expansions m,
    -- | Similar to expansions, but are always applied after the expansion. The
    -- purpose of the name is just to facilitate binding/unbinding and control
    -- execution order, it respects lexicographic order on keys.
    filters :: Filters m
  }
  deriving (Generic)

instance Monoid (OndimState m) where
  mempty = OndimState mempty mempty

instance Semigroup (OndimState m) where
  OndimState x1 y1 <> OndimState x2 y2 = OndimState (x1 <> x2) (y1 <> y2)

-- | Ondim's global state
data OndimGS = OndimGS
  { expansionDepth :: Int,
    expansionTrace :: [Text]
  }
  deriving (Read, Show, Generic)

-- * Exceptions

data OndimException
  = MaxExpansionDepthExceeded [Text]
  | ExpansionNotBound Text [Text]
  | CustomException Text [Text]
  deriving (Show)

-- * Class

class
  ( HasSub GSubTag (ExpTypes t) t,
    AllMods CanLift (ExpTypes t),
    AllMods (Substructure t) (ExpTypes t),
    Typeable t
  ) =>
  OndimNode t
  where
  type ExpTypes t :: [SubSpec]
  identify :: t -> Maybe Text
  identify _ = Nothing
  fromText :: Maybe (Text -> [t])
  fromText = Nothing
  attributes :: Monad m => t -> Ondim m [Attribute]
  attributes _ = pure []

-- * Combinators

data OCTag

type HasSub tag ls t = HS.HasSub OCTag tag ls t
type Carrier a = HS.Carrier OCTag a
type ToSpec a = HS.ToSpec OCTag a
type ToSpecSel s a = HS.ToSpecSel OCTag s a

-- Classes

class CanLift (s :: Type) where
  liftSub ::
    Monad m =>
    Carrier s ->
    Ondim m (Carrier s)

class Substructure (a :: Type) (s :: Type) where
  getSubs :: Carrier s -> [a]
  modSubs :: Applicative m => ([a] -> m [a]) -> Carrier s -> m (Carrier s)

instance {-# OVERLAPPABLE #-} Substructure a s where
  getSubs = mempty
  modSubs = const pure

instance (Carrier a ~ [a]) => Substructure a a where
  getSubs = id
  modSubs = id

-- * Lifting functions

modSubLift ::
  forall ls m t.
  ( Monad m,
    HasSub GSubTag ls t,
    AllMods CanLift ls
  ) =>
  t ->
  Ondim m t
modSubLift = HS.modSub @OCTag @GSubTag @ls @t (Proxy @CanLift) (\(_ :: Proxy s) -> liftSub @s)
{-# INLINEABLE modSubLift #-}

-- * Structure functions

getSubstructure' ::
  forall a ls t.
  ( HasSub GSubTag ls t,
    AllMods (Substructure a) ls
  ) =>
  t ->
  [a]
getSubstructure' = HS.getSub @OCTag @GSubTag @ls @t (Proxy @(Substructure a)) (\(_ :: Proxy j) -> getSubs @a @j)

getSubstructure ::
  forall a t.
  ( OndimNode t,
    AllMods (Substructure a) (ExpTypes t)
  ) =>
  t ->
  [a]
getSubstructure = getSubstructure' @a @(ExpTypes t)

modSubstructureM' ::
  forall a ls t m.
  ( HasSub GSubTag ls t,
    AllMods (Substructure a) ls,
    Applicative m
  ) =>
  ([a] -> m [a]) ->
  t ->
  m t
modSubstructureM' f = HS.modSub @OCTag @GSubTag @ls @t (Proxy @(Substructure a)) (\(_ :: Proxy j) -> modSubs @a @j f)

modSubstructureM ::
  forall a t m.
  ( OndimNode t,
    AllMods (Substructure a) (ExpTypes t),
    Applicative m
  ) =>
  ([a] -> m [a]) ->
  t ->
  m t
modSubstructureM = modSubstructureM' @a @(ExpTypes t)

-- * Attributes

-- | Alias for attributes
type Attribute = (Text, Text)
