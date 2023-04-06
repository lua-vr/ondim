{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
type Filters m = HashMap Text (SomeFilter m)
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
    -- | Similar to expansions, but are always applied after the expansion (the
    -- purpose of the name is just to facilitate binding/unbinding).
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
  getAttrs :: t -> [Attribute]
  default getAttrs ::
    ( OndimNode t,
      AllMods (Substructure Attribute) (ExpTypes t)
    ) =>
    t ->
    [Attribute]
  getAttrs = getSubstructure @Attribute

-- * Combinators

data OCTag

type family CombinatorCarrier (b :: Type) :: Type where
  CombinatorCarrier (Identity a) = [a]
  CombinatorCarrier (Under b s a) = b
  CombinatorCarrier (MatchWith s a) = s
  CombinatorCarrier (OneSub a) = a
  CombinatorCarrier (Converting b a) = b
  CombinatorCarrier a = [a]

type instance HS.Carrier OCTag a = CombinatorCarrier a
type instance HS.Carrier OCTag a = CombinatorCarrier a

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

-- Definitions

{- | Use this for matching with another type that is coercible to the functor
you want.
-}
data MatchWith (s :: Type) (a :: Type)

instance
  ( CanLift a,
    Coercible (Carrier a) b
  ) =>
  CanLift (MatchWith b a)
  where
  liftSub = fmap coerce . liftSub @a . coerce

instance
  ( Substructure s a,
    Coercible (Carrier a) b
  ) =>
  Substructure s (MatchWith b a)
  where
  getSubs = getSubs @s @a . coerce
  modSubs f = fmap coerce . modSubs @s @a f . coerce

{- | If your type is not within a list but is a monoid, you can use this and
pretend it's a list.
-}
data OneSub (a :: Type)

instance
  ( CanLift a,
    Carrier a ~ [a],
    Monoid a
  ) =>
  CanLift (OneSub a)
  where
  liftSub (x :: a) = mconcat <$> liftSub @a [x]

{- | Use this for matching a subcomponent nested inside another type. Useful if
you don't want to add the middle type to the list of expansible types.
-}
data Under (b :: Type) (s :: SelSpec) (a :: Type)

instance
  ( CanLift a,
    HasSub GSubTag '[ 'SubSpec s a (Carrier a)] b
  ) =>
  CanLift (Under b s a)
  where
  liftSub = modSubLift @'[ 'SubSpec s a (Carrier a)]

instance
  ( Substructure k a,
    HasSub GSubTag '[ 'SubSpec s a (Carrier a)] b
  ) =>
  Substructure k (Under b s a)
  where
  getSubs = getSubstructure' @k @'[ 'SubSpec s a (Carrier a)]
  modSubs = modSubstructureM' @k @'[ 'SubSpec s a (Carrier a)]

data Converting a b

class Conversible a b where
  convertTo :: a -> b
  convertFrom :: b -> a

instance
  ( CanLift a,
    Conversible s (Carrier a)
  ) =>
  CanLift (Converting s a)
  where
  liftSub = fmap convertFrom . liftSub @a . convertTo

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
  ( HasSub GSubTag (ExpTypes t) t,
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
  ( HasSub GSubTag (ExpTypes t) t,
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
