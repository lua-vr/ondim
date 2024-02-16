{-# LANGUAGE UndecidableInstances #-}

module Ondim.MultiWalk.Combinators
  ( -- ** Spec
    ToSpec,
    ToSpecSel,
    Spec (..),
    SubSpec (..),
    SelSpec (..),
    Carrier,

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
    Expansible (..),
    Substructure (..),
    AllMods,
    NLDef,
  )
where

import Control.MultiWalk.HasSub (AllMods, GSubTag, SelSpec, Spec (..), SubSpec (..))
import Control.MultiWalk.HasSub qualified as HS
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class (OndimNode (..), Expansible (..), Substructure (..))
import Ondim.MultiWalk.Core
import Ondim.MultiWalk.Substructure
import Type.Errors qualified as TE

type family Carrier (b :: Type) :: Type where
  Carrier (NodeList a) = [a]
  Carrier (NLDef a) = [a]
  Carrier (Nesting b) = b
  Carrier (MatchWith s _) = s
  Carrier (OneSub a) = a
  Carrier (Trav f a) = f (Carrier a)
  Carrier (Converting b _) = b
  Carrier (Sequence a _) = Carrier a
  Carrier (Custom b tag) = b
  Carrier (ModSub b _) = b
  Carrier a = TE.TypeError ('TE.Text "The type " 'TE.:<>: TE.ShowTypeQuoted a 'TE.:<>: 'TE.Text " is not a valid combinator.")

type instance HS.Carrier OCTag a = Carrier a

-- Definitions

data NLDef (a :: Type)

instance OndimNode a => Expansible (NLDef a) where
  expandSpec = foldMapM expandNode

instance OndimNode a => Substructure a (NLDef a) where
  getSub = id

-- | Shorthand for 'NodeList'
type NL = NodeList

-- | @'NodeList' a@ matches @[a]@ where @a@ is a 'OndimNode'
data NodeList (a :: Type)

instance OndimNode a => Expansible (NodeList a) where
  expandSpec = expandNodes

instance OndimNode a => Substructure a (NodeList a) where
  getSub = id

-- | @'MatchWith' s a@ matches a type @s@ that is coercible to the 'Carrier' of @a@
data MatchWith (s :: Type) (a :: Type)

instance
  ( Expansible a,
    Coercible (Carrier a) b
  ) =>
  Expansible (MatchWith b a)
  where
  expandSpec = fmap coerce . expandSpec @a . coerce

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
  ( Expansible a,
    Carrier a ~ [a],
    Monoid a
  ) =>
  Expansible (OneSub a)
  where
  expandSpec (x :: a) = mconcat <$> expandSpec @a [x]

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
  ( Expansible a,
    Traversable f
  ) =>
  Expansible (Trav f a)
  where
  expandSpec = traverse (expandSpec @a)

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
  Expansible (Nesting b)
  where
  expandSpec = expandSubstructures

instance
  ( OndimNode b,
    AllMods (Substructure k) (ExpTypes b)
  ) =>
  Substructure k (Nesting b)
  where
  getSub = getSubstructure

data Converting (a :: Type) (b :: Type)

class Conversible a b where
  convertTo :: a -> b
  updateFrom :: a -> b -> a

instance
  ( Expansible a,
    Conversible s (Carrier a)
  ) =>
  Expansible (Converting s a)
  where
  expandSpec x = fmap (updateFrom x) $ expandSpec @a $ convertTo x

instance
  ( Substructure k a,
    Conversible s (Carrier a)
  ) =>
  Substructure k (Converting s a)
  where
  getSub = getSub @k @a . convertTo

data Sequence (a :: Type) (b :: Type)

instance
  ( Carrier a ~ Carrier b,
    Expansible a,
    Expansible b
  ) =>
  Expansible (Sequence a b)
  where
  expandSpec = expandSpec @a >=> expandSpec @b

instance
  ( Carrier a ~ Carrier b,
    Substructure k a,
    Substructure k b
  ) =>
  Substructure k (Sequence a b)
  where
  getSub x = getSub @k @a x <> getSub @k @b x

data Custom (b :: Type) (tag :: Type)

instance OndimNode t => Substructure t (Custom [t] tag) where
  getSub = id

data ModSub (needle :: Type) (spec :: [SubSpec])

instance
  ( AllMods Expansible ('SpecList spec),
    HasSub GSubTag ('SpecList spec) needle
  ) =>
  Expansible (ModSub needle (spec :: [SubSpec]))
  where
  expandSpec = expandSpecList @('SpecList spec)

instance
  ( AllMods (Substructure t) ('SpecList spec),
    HasSub GSubTag ('SpecList spec) needle
  ) =>
  Substructure t (ModSub needle (spec :: [SubSpec]))
  where
  getSub = getSubstructure' @t @('SpecList spec)
