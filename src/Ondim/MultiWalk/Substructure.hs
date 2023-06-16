{-# LANGUAGE AllowAmbiguousTypes #-}

module Ondim.MultiWalk.Substructure where

import Control.MultiWalk.HasSub (AllMods, GSubTag)
import Control.MultiWalk.HasSub qualified as HS
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class

-- * Class

class Substructure (a :: Type) (s :: Type) where
  getSubs :: Carrier s -> [a]
  modSubs :: Applicative m => ([a] -> m [a]) -> Carrier s -> m (Carrier s)

instance {-# OVERLAPPABLE #-} Substructure a s where
  getSubs = mempty
  modSubs = const pure

instance (Carrier a ~ [a]) => Substructure a a where
  getSubs = id
  modSubs = id

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

modSubstructureM' ::
  forall a ls t m.
  ( HasSub GSubTag ls t,
    AllMods (Substructure a) ls,
    Applicative m
  ) =>
  ([a] -> m [a]) ->
  t ->
  m t
modSubstructureM' f = HS.modSub @OCTag @GSubTag @ls @t (Proxy @(Substructure a)) (\(_ :: Proxy j) -> modSubs @a @j f)

modSubstructureM ::
  forall a t m.
  ( OndimNode t,
    AllMods (Substructure a) (ExpTypes t),
    Applicative m
  ) =>
  ([a] -> m [a]) ->
  t ->
  m t
modSubstructureM = modSubstructureM' @a @(ExpTypes t)

modSubstructure ::
  ( OndimNode t,
    AllMods (Substructure a) (ExpTypes t)
  ) =>
  ([a] -> [a]) ->
  t ->
  t
modSubstructure f = runIdentity . modSubstructureM (pure . f)
