{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Ondim.MultiWalk.Combinators where

import Control.MultiWalk.HasSub (SubSpec)
import Type.Errors qualified as TE
import {-# SOURCE #-} Ondim.MultiWalk.Class (OndimNode, CanLift)

instance OndimNode a => CanLift (NLDef a)
instance OndimNode a => CanLift (NodeList a)
instance (CanLift a, Traversable f) => CanLift (Trav f a)

data NLDef (a :: Type)
type role NLDef phantom

data NodeList (a :: Type)
type role NodeList phantom

data Nesting (b :: Type)
type role Nesting phantom

data Converting (a :: Type) (b :: Type)
type role Converting phantom phantom

data Sequence (a :: Type) (b :: Type)
type role Sequence phantom phantom

data Custom (b :: Type) (tag :: Type)
type role Custom phantom phantom

data ModSub (needle :: Type) (spec :: [SubSpec])
type role ModSub phantom phantom

data MatchWith (s :: Type) (a :: Type)
type role MatchWith phantom phantom

data OneSub (a :: Type)
type role OneSub phantom

data Trav (f :: Type -> Type) (a :: Type)
type role Trav phantom phantom

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
