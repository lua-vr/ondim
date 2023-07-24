{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Class (OndimNode (..), ondimCast) where

import Control.MultiWalk.HasSub (AllMods, GSubTag, Spec (..))
import Data.Typeable (eqT, (:~:) (..))
import Ondim.MultiWalk.Basic
import {-# SOURCE #-} Ondim.MultiWalk.Core

-- * Class

class
  ( HasSub GSubTag (ExpTypes t) t,
    AllMods CanLift (ExpTypes t),
    OndimCast t,
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
  renderNode :: Maybe (t -> LByteString)
  renderNode = Nothing
  nodeAsText :: Maybe (t -> Text)
  nodeAsText = Nothing

-- Some data instances (won't lift)

instance OndimNode Text where
  type ExpTypes Text = 'SpecLeaf
  nodeAsText = Just id

instance OndimNode LByteString where
  type ExpTypes LByteString = 'SpecLeaf

instance OndimNode (Text, Text) where
  type ExpTypes (Text, Text) = 'SpecLeaf

class Typeable a => OndimCast a where
  ondimCast :: OndimNode b => Maybe (a -> [b])

instance {-# OVERLAPPABLE #-} (Typeable a) => OndimCast a where
  ondimCast :: forall b. OndimNode b => Maybe (a -> [b])
  ondimCast = castFrom Proxy

instance (OndimCast a) => OndimCast [a] where
  ondimCast :: forall b. OndimNode b => Maybe ([a] -> [b])
  ondimCast
    | Just Refl <- eqT @b @a = Just id
    | otherwise = castFrom Proxy <|> foldMap' <$> ondimCast

-- ondimCast :: forall a b. (OndimNode a, OndimNode b) => Maybe (a -> [b])
-- ondimCast = castTo (Proxy @b) <|> castFrom (Proxy @a)
