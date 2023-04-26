{-# LANGUAGE UndecidableInstances #-}

module Ondim.MultiWalk.Combinators
  ( -- Spec
    ToSpec,
    ToSpecSel,
    SubSpec (..),
    SelSpec (..),
    -- Combinators
    Under,
    MatchWith,
    Conversible (..),
    Converting,
    Trav,
    OneSub,
  )
where

import Control.MultiWalk.HasSub (GSubTag, SelSpec, SubSpec (..))
import Control.MultiWalk.HasSub qualified as HS
import Ondim.MultiWalk.Basic

type family CombinatorCarrier (b :: Type) :: Type where
  CombinatorCarrier (Under b s a) = b
  CombinatorCarrier (MatchWith s a) = s
  CombinatorCarrier (OneSub a) = a
  CombinatorCarrier (Trav f a) = f (Carrier a)
  CombinatorCarrier (Converting b a) = b
  CombinatorCarrier a = [a]

type instance HS.Carrier OCTag a = CombinatorCarrier a
type instance HS.Carrier OCTag a = CombinatorCarrier a

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

instance
  ( Substructure k a,
    Carrier a ~ [a],
    Monoid a
  ) =>
  Substructure k (OneSub a)
  where
  getSubs = getSubs @k @a . one
  modSubs f (x :: a) = mconcat <$> modSubs @k @a f [x]

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
  modSubs f = traverse (modSubs @s @a f)

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

instance
  ( Substructure k a,
    Conversible s (Carrier a)
  ) =>
  Substructure k (Converting s a)
  where
  getSubs = getSubs @k @a . convertTo
  modSubs f = fmap convertFrom . modSubs @k @a f . convertTo
