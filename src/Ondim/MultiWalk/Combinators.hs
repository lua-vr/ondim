{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ondim.MultiWalk.Combinators where

import Prelude hiding (All)
import Control.MultiWalk.HasSub (SubSpec (..), GSubTag, HasSub)
import Data.HashMap.Strict qualified as Map
import Ondim.MultiWalk.Core (CanLift (..), modSubLift, Substructure (..), getSubstructure', modSubstructureM')

-- * Instances

type family ToSpecList (xs :: [Type]) :: [SubSpec] where
  ToSpecList (x : xs) = BuildSpec x : ToSpecList xs
  ToSpecList '[] = '[]

type BuildSpec x = 'SubSpec (CombinatorCarrier x) x

type family CombinatorCarrier (b :: Type) :: Type where
  CombinatorCarrier (Under s a) = s
  CombinatorCarrier (MatchWith s a) = s
  CombinatorCarrier (PairSub k v) = HashMap k v
  CombinatorCarrier (OneSub a) = a
  CombinatorCarrier (Converting s a) = s
  CombinatorCarrier a = [a]

-- | Use this for matching with another type that is coercible to the functor you want.
data MatchWith (s :: Type) (a :: Type)

instance
  ( CanLift (BuildSpec a),
    Coercible (CombinatorCarrier a) c,
    c ~ CombinatorCarrier (MatchWith b a)
  ) =>
  CanLift ('SubSpec c (MatchWith b a))
  where
  liftSub = fmap coerce . liftSub @(BuildSpec a) . coerce

instance
  ( Substructure s (BuildSpec a),
    Coercible (CombinatorCarrier a) c,
    c ~ CombinatorCarrier (MatchWith b a)
  ) =>
  Substructure s ('SubSpec c (MatchWith b a))
  where
  getSubs = getSubs @s @(BuildSpec a) . coerce
  modSubs f = fmap coerce . modSubs @s @(BuildSpec a) f . coerce

-- | Use this for matching a subcomponent nested inside another type. Useful if
-- you don't want to add the middle type to the list of walkable types.
data Under (b :: Type) (a :: Type)

instance
  ( CanLift (BuildSpec a),
    HasSub GSubTag '[BuildSpec a] b
  ) =>
  CanLift ('SubSpec b (Under b a))
  where
  liftSub = modSubLift @'[BuildSpec a]

instance
  ( Substructure s (BuildSpec a),
    HasSub GSubTag '[BuildSpec a] b
  ) =>
  Substructure s ('SubSpec b (Under b a))
  where
  getSubs = getSubstructure' @s @'[BuildSpec a]
  modSubs = modSubstructureM' @s @'[BuildSpec a]


data PairSub k v

instance
  ( c ~ CombinatorCarrier (PairSub k v),
    Hashable k,
    CanLift (BuildSpec (k, v))
  ) =>
  CanLift ('SubSpec c (PairSub k v))
  where
  liftSub (x :: HashMap k v) =
    Map.fromList <$> liftSub @(BuildSpec (k, v)) (Map.toList x)

data OneSub a

instance
  ( c ~ CombinatorCarrier (OneSub a),
    CanLift ('SubSpec [a] a),
    Monoid a
  ) =>
  CanLift ('SubSpec c (OneSub a))
  where
  liftSub (x :: a) = mconcat <$> liftSub @('SubSpec [a] a) [x]

class Conversible a b where
  convertTo :: a -> b
  convertFrom :: b -> a

instance
  ( CanLift (BuildSpec a),
    Conversible s (CombinatorCarrier a),
    c ~ CombinatorCarrier (Converting s a)
  ) =>
  CanLift ('SubSpec c (Converting s a))
  where
  liftSub = fmap convertFrom . liftSub @(BuildSpec a) . convertTo

data Converting a b
