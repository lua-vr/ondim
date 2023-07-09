{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.MultiWalk.Combinators
  ( -- Spec
    ToSpec,
    ToSpecSel,
    Spec (..),
    SubSpec (..),
    SelSpec (..),
    -- Combinators
    Nesting,
    MatchWith,
    Conversible (..),
    Converting,
    Trav,
    OneSub,
    Sequence,
  )
where

import Control.MultiWalk.HasSub (AllMods, SelSpec, Spec (..), SubSpec (..))
import Control.MultiWalk.HasSub qualified as HS
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class (OndimNode (..))
import Ondim.MultiWalk.Core
import Ondim.MultiWalk.Substructure

type family CombinatorCarrier (b :: Type) :: Type where
  CombinatorCarrier (Nesting b) = b
  CombinatorCarrier (MatchWith s _) = s
  CombinatorCarrier (OneSub a) = a
  CombinatorCarrier (Trav f a) = f (Carrier a)
  CombinatorCarrier (Converting b _) = b
  CombinatorCarrier (Sequence a _) = Carrier a
  CombinatorCarrier (List a) = [a]
  CombinatorCarrier a = [a]

type instance HS.Carrier OCTag a = CombinatorCarrier a

-- Definitions

data List a

instance (OndimNode a) => CanLift (List a) where
  liftSub = liftNodes

instance (OndimNode a) => OndimNode [a] where
  type ExpTypes [a] = 'SpecSelf (List a)
  castFrom p = (one .) <$> castFrom p
  castTo (p :: Proxy t)
    | Just Refl <- eqT @t @a = Just id
    | otherwise = foldMap' <$> castTo p

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

instance
  ( Substructure k a,
    Carrier a ~ [a],
    Monoid a
  ) =>
  Substructure k (OneSub a)
  where
  getSubs = getSubs @k @a . one

-- | Use this for matching with a type inside a traversable functor.
data Trav (f :: Type -> Type) (a :: Type)

instance
  ( CanLift a,
    Traversable f
  ) =>
  CanLift (Trav f a)
  where
  liftSub = traverse (liftSub @a)

instance
  ( Substructure s a,
    Traversable f
  ) =>
  Substructure s (Trav f a)
  where
  getSubs = foldMap (getSubs @s @a)

-- | Use this for matching a subcomponent nested inside another type.
data Nesting (b :: Type)

instance
  ( OndimNode b
  ) =>
  CanLift (Nesting b)
  where
  liftSub = liftSubstructures

instance
  ( OndimNode b,
    AllMods (Substructure k) (ExpTypes b)
  ) =>
  Substructure k (Nesting b)
  where
  getSubs = getSubstructure

data Converting a b

class Conversible a b where
  convertTo :: a -> b
  updateFrom :: a -> b -> a

instance
  ( CanLift a,
    Conversible s (Carrier a)
  ) =>
  CanLift (Converting s a)
  where
  liftSub x = fmap (updateFrom x) $ liftSub @a $ convertTo x

instance
  ( Substructure k a,
    Conversible s (Carrier a)
  ) =>
  Substructure k (Converting s a)
  where
  getSubs = getSubs @k @a . convertTo

data Sequence a b

instance
  ( Carrier a ~ Carrier b,
    CanLift a,
    CanLift b
  ) =>
  CanLift (Sequence a b)
  where
  liftSub = liftSub @a >=> liftSub @b

instance
  ( Carrier a ~ Carrier b,
    Substructure k a,
    Substructure k b
  ) =>
  Substructure k (Sequence a b)
  where
  getSubs x = getSubs @k @a x <> getSubs @k @b x

-- Somewhat lost instances

instance OndimNode Attribute where
  type ExpTypes Attribute = 'SpecList '[ToSpec (OneSub Text)]
  identify ~(name, _) = T.stripPrefix "e:" name
