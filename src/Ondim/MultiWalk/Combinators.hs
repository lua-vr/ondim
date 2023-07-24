{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.MultiWalk.Combinators
  ( -- ** Spec
    ToSpec,
    ToSpecSel,
    Spec (..),
    SubSpec (..),
    SelSpec (..),

    -- ** Combinators
    NL,
    NodeList,
    Nesting,
    MatchWith,
    Conversible (..),
    Converting,
    Trav,
    OneSub,
    Sequence,
    Custom,
    ModSub,

    -- ** Internal
    CanLift (..),
    Substructure (..),
    AllMods,
  )
where

import Control.MultiWalk.HasSub (AllMods, GSubTag, SelSpec, Spec (..), SubSpec (..))
import Control.MultiWalk.HasSub qualified as HS
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class (OndimNode (..))
import Ondim.MultiWalk.Core
import Ondim.MultiWalk.Substructure
import Type.Errors qualified as TE

type family CombinatorCarrier (b :: Type) :: Type where
  CombinatorCarrier (NodeList a) = [a]
  CombinatorCarrier (Nesting b) = b
  CombinatorCarrier (MatchWith s _) = s
  CombinatorCarrier (OneSub a) = a
  CombinatorCarrier (Trav f a) = f (Carrier a)
  CombinatorCarrier (Converting b _) = b
  CombinatorCarrier (Sequence a _) = Carrier a
  CombinatorCarrier (Custom b tag) = b
  CombinatorCarrier (ModSub b _) = b
  CombinatorCarrier a = TE.TypeError ('TE.Text "The type " 'TE.:<>: TE.ShowTypeQuoted a 'TE.:<>: 'TE.Text " is not a valid combinator.")

type instance HS.Carrier OCTag a = CombinatorCarrier a

-- Definitions

-- | Shorthand for 'NodeList'
type NL = NodeList

-- | @'NodeList' a@ matches @[a]@ where @a@ is a 'OndimNode'
data NodeList a

instance (OndimNode a) => CanLift (NodeList a) where
  liftSub = liftNodes

instance Substructure a (NodeList a) where
  getSub = id

instance (OndimNode a) => OndimNode [a] where
  type ExpTypes [a] = 'SpecSelf (NodeList a)
  castFrom p = (one .) <$> castFrom p
  renderNode = foldMap' <$> renderNode
  nodeAsText = foldMap' <$> nodeAsText

-- | @'MatchWith' s a@ matches a type @s@ that is coercible to the 'Carrier' of @a@
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
  getSub = getSub @s @a . coerce

{- | If your type is not within a NodeList but is a monoid, you can use this and
pretend it's a NodeList.
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
  getSub = getSub @k @a . one

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
  getSub = foldMap (getSub @s @a)

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
  getSub = getSubstructure

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
  getSub = getSub @k @a . convertTo

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
  getSub x = getSub @k @a x <> getSub @k @b x

data Custom b tag

data ModSub needle (spec :: [SubSpec])

instance
  ( AllMods CanLift ('SpecList spec),
    HasSub GSubTag ('SpecList spec) needle
  ) =>
  CanLift (ModSub needle (spec :: [SubSpec]))
  where
  liftSub = modSubLift @('SpecList spec)

instance
  ( AllMods (Substructure t) ('SpecList spec),
    HasSub GSubTag ('SpecList spec) needle
  ) =>
  Substructure t (ModSub needle (spec :: [SubSpec]))
  where
  getSub = getSubstructure' @t @('SpecList spec)
