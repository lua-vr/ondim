{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Class (OndimNode (..), ondimCast) where

import Control.MultiWalk.HasSub (AllMods, GSubTag, Spec (..))
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
  type ExpTypes t :: Spec
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
  type ExpTypes Text = 'SpecLeaf

instance OndimNode LByteString where
  type ExpTypes LByteString = 'SpecLeaf

ondimCast :: forall a b. (OndimNode a, OndimNode b) => Maybe (a -> [b])
ondimCast = castFrom (Proxy @a) <|> castTo (Proxy @b)
