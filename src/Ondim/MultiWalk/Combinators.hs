{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ondim.MultiWalk.Combinators where

import Prelude hiding (All)
import Control.MultiWalk.HasSub (SubSpec (..), GSubTag, HasSub)
import Data.Map qualified as Map
import Ondim.MultiWalk.Core (CanLift (..), modSubLift, Substructure (..), getSubstructure', modSubstructureM')

-- * Instances

type family ToSpecList (xs :: [Type]) :: [SubSpec] where
  ToSpecList (x : xs) = BuildSpec x : ToSpecList xs
  ToSpecList '[] = '[]

type BuildSpec x = 'SubSpec (CombinatorCarrier x) x

type family CombinatorCarrier (b :: Type) :: Type where
  CombinatorCarrier (Under s a) = s
  CombinatorCarrier (MatchWith s a) = s
  CombinatorCarrier (PairSub k v) = Map k v
  CombinatorCarrier (OneSub a) = a
  CombinatorCarrier (Converting s a) = s
  CombinatorCarrier a = [a]

-- | Use this for matching with another type that is coercible to the functor you want.
data MatchWith (s :: Type) (a :: Type)

instance
  ( CanLift tag (BuildSpec a),
    Coercible (CombinatorCarrier a) c,
    c ~ CombinatorCarrier (MatchWith b a)
  ) =>
  CanLift tag ('SubSpec c (MatchWith b a))
  where
  liftSub = fmap coerce . liftSub @tag @(BuildSpec a) . coerce

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
  ( CanLift tag (BuildSpec a),
    HasSub GSubTag '[BuildSpec a] b
  ) =>
  CanLift tag ('SubSpec b (Under b a))
  where
  liftSub = modSubLift @tag @'[BuildSpec a]

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
    Ord k,
    CanLift tag (BuildSpec (k, v))
  ) =>
  CanLift tag ('SubSpec c (PairSub k v))
  where
  liftSub (x :: Map k v) =
    Map.fromList <$> liftSub @tag @(BuildSpec (k, v)) (Map.toList x)

data OneSub a

instance
  ( c ~ CombinatorCarrier (OneSub a),
    CanLift tag ('SubSpec [a] a),
    Monoid a
  ) =>
  CanLift tag ('SubSpec c (OneSub a))
  where
  liftSub (x :: a) = mconcat <$> liftSub @tag @('SubSpec [a] a) [x]

class Conversible tag a b where
  convertTo :: a -> b
  convertFrom :: b -> a

instance
  ( CanLift tag (BuildSpec a),
    Conversible tag s (CombinatorCarrier a),
    c ~ CombinatorCarrier (Converting s a)
  ) =>
  CanLift tag ('SubSpec c (Converting s a))
  where
  liftSub = fmap (convertFrom @tag) . liftSub @tag @(BuildSpec a) . convertTo @tag

data Converting a b
