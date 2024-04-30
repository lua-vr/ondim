{-# LANGUAGE AllowAmbiguousTypes #-}

module Ondim.Expansible.Generic where

import Control.MultiWalk.HasSub (AllMods, GSubTag)
import Control.MultiWalk.HasSub qualified as HS
import Ondim (Ondim)
import Ondim.Advanced (Carrier, Expansible (..), Substructure (..))

-- * Combinators

data OCTag

type HasSub tag ls t = HS.HasSub OCTag tag ls t
type ToSpec a = HS.ToSpec OCTag a
type ToSpecSel s a = HS.ToSpecSel OCTag s a
type instance HS.Carrier OCTag a = Carrier a

-- | Expand a list of nodes according to a spec list.
expandSpecList ::
  forall ls t s.
  ( HasSub GSubTag ls t,
    AllMods Expansible ls
  ) =>
  t ->
  Ondim s t
expandSpecList = HS.modSub @OCTag @GSubTag @ls @t (Proxy @Expansible) (\(_ :: Proxy r) -> expandSpec @r)
{-# INLINEABLE expandSpecList #-}

-- * Structure functions

getSubstructure ::
  forall a ls t.
  ( HasSub GSubTag ls t,
    AllMods (Substructure a) ls
  ) =>
  t ->
  [a]
getSubstructure = HS.getSub @OCTag @GSubTag @ls @t (Proxy @(Substructure a)) (\(_ :: Proxy j) -> getSub @a @j)
