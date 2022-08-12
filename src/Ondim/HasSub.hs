{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
-- | 

module Ondim.HasSub where
import GHC.Generics
import GHC.TypeLits (TypeError, ErrorMessage(..))

class HasSub tag t s where
  getSubs :: t -> [s]
  setSubs :: t -> [s] -> t
  default getSubs :: (Generic t, HasSub' [s] (Rep t), ContainsSome [s] (Rep t)) => t -> [s]
  getSubs = getSubs' . from
  default setSubs :: (Generic t, HasSub' [s] (Rep t), ContainsSome [s] (Rep t)) => t -> [s] -> t
  setSubs x = to . setSubs' (from x)

instance HasSub tag [t] t

newtype OneSub a = OneSub a
  deriving Generic

instance (Generic t, HasSub' s (Rep t), ContainsSome s (Rep t)
         , Monoid s
         ) => HasSub tag t (OneSub s) where
  getSubs = one . coerce @s . getSubs' . from
  setSubs x s = to $ setSubs' (from x) $ (mconcat (coerce @[OneSub s] @[s] s))

newtype NestedSub (b :: Type) a = NestedSub a
  deriving Generic

instance (Generic t, HasSub' u (Rep t), ContainsSome u (Rep t)
         , HasSub tag u s
         ) => HasSub tag t (NestedSub u s) where
  getSubs = coerce . getSubs @tag @u @s . getSubs' . from
  setSubs x s =
    to $ setSubs' (from x) $ (setSubs @tag @u @s (getSubs' $ from x) (coerce s))

class HasSub' s t where
  getSubs' :: t p -> s
  setSubs' :: t p -> s -> t p

instance {-# OVERLAPPING #-} HasSub' s (K1 j s) where
  getSubs' (K1 x) = x
  setSubs' _ x = K1 x

instance Monoid s => HasSub' s (K1 j t) where
  getSubs' = const mempty
  setSubs' = const

instance {-# OVERLAPPING #-} AtMostOne s xs => HasSub' s (S1 j (K1 k s) :*: xs) where
  getSubs' (M1 (K1 x) :*: _) = x
  setSubs' (_ :*: y) x = M1 (K1 x) :*: y

associate :: (:*:) (f1 :*: f2) g p -> (:*:) f1 (f2 :*: g) p
associate ((x :*: y) :*: z) = x :*: y :*: z

unassociate :: (:*:) f (g1 :*: g2) p -> (:*:) (f :*: g1) g2 p
unassociate (x :*: y :*: z) = (x :*: y) :*: z

instance {-# OVERLAPPING #-} HasSub' s (xs :*: ys :*: zs) => HasSub' s ((xs :*: ys) :*: zs) where
  getSubs' x = getSubs' (associate x)
  setSubs' x s = unassociate $ setSubs' (associate x) s

instance HasSub' s y => HasSub' s (x :*: y) where
  getSubs' (_ :*: y) = getSubs' y
  setSubs' (x :*: y) y' = x :*: setSubs' y y'

instance Monoid s => HasSub' s U1 where
  getSubs' = const mempty
  setSubs' = const

instance (HasSub' s x, HasSub' s y) => HasSub' s (x :+: y) where
  getSubs' (L1 x) = getSubs' x
  getSubs' (R1 y) = getSubs' y
  setSubs' (L1 x) x' = L1 $ setSubs' x x'
  setSubs' (R1 y) y' = R1 $ setSubs' y y'

instance (HasSub' s x) => HasSub' s (M1 i j x) where
  getSubs' (M1 x) = getSubs' x
  setSubs' (M1 x) x' = M1 $ setSubs' x x'

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
  Or () b       = ()
  Or a ()       = ()

type family AtMostOne s t :: Constraint where
  AtMostOne s (S1 _ (K1 _ s)) =
    TypeError ('Text "At most one field of type "
                ':<>: 'ShowType s ':<>:
                'Text " in each constructor is allowed."
              )
  AtMostOne s (xs :*: ys) = AtMostOne s xs `Or` AtMostOne s ys
  AtMostOne s xs = ()

type family ClearProd s t :: Constraint where
  ClearProd s (S1 i (K1 j s)) = ()
  ClearProd s (xs :*: ys) = ClearProd s xs `Or` ClearProd s ys

type family ClearSum s t :: Constraint where
  ClearSum s (C1 _ xs) = ClearProd s xs
  ClearSum s (xs :+: ys) = ClearSum s xs `Or` ClearSum s ys

type family ContainsSome s t :: Constraint where
  ContainsSome s (D1 _ xs) = ClearSum s xs
    `Or`
    TypeError ('Text "Type does not contain any constructor with a field of type "
                ':<>: 'ShowType s
              )
