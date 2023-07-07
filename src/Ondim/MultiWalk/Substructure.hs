{-# LANGUAGE AllowAmbiguousTypes #-}

module Ondim.MultiWalk.Substructure where

import Control.MultiWalk.HasSub (AllMods, GSubTag)
import Control.MultiWalk.HasSub qualified as HS
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class

-- * Class

class Substructure (target :: Type) (needle :: Type) where
  getSubs :: Carrier needle -> [target]

instance {-# OVERLAPPABLE #-} Substructure target needle where
  getSubs = mempty

instance (Carrier a ~ [a]) => Substructure a a where
  getSubs = id

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
  ( OndimNode t,
    AllMods (Substructure a) (ExpTypes t)
  ) =>
  t ->
  [a]
getSubstructure = getSubstructure' @a @(ExpTypes t)
