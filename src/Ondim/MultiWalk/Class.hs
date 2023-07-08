{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Ondim.MultiWalk.Class where

import Control.MultiWalk.HasSub (AllMods, GSubTag, SubSpec)
import Ondim.MultiWalk.Basic
import {-# SOURCE #-} Ondim.MultiWalk.Core

-- * Class

class
  ( HasSub GSubTag (ExpTypes t) t,
    AllMods CanLift (ExpTypes t),
    Typeable t
  ) =>
  OndimNode t
  where
  type ExpTypes t :: [SubSpec]
  identify :: t -> Maybe Text
  identify _ = Nothing
  attributes :: Monad m => t -> Ondim m [Attribute]
  attributes _ = pure []
  children :: t -> [t]
  children _ = []
  castFrom :: Typeable a => Proxy a -> Maybe (a -> [t])
  castFrom _ = Nothing
  castTo :: Typeable a => Proxy a -> Maybe (t -> [a])
  castTo _ = Nothing

instance OndimNode Text where
  type ExpTypes Text = '[]

class Typeable a => OndimCast a where
  ondimCast :: OndimNode b => Maybe (a -> [b])

instance {-# OVERLAPPABLE #-} (OndimNode a, Typeable a) => OndimCast a where
  ondimCast :: forall b. OndimNode b => Maybe (a -> [b])
  ondimCast = castFrom (Proxy @a) <|> castTo (Proxy @b)

instance OndimCast a => OndimCast [a] where
  ondimCast = do
    f <- ondimCast @a
    return (foldMap f)
