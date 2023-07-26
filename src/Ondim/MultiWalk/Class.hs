{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Class
  ( OndimNode (..),
    CanLift (..),
    Substructure (..),
    ondimCast,
  ) where

import Control.MultiWalk.HasSub (AllMods, GSubTag, Spec (..))
import Data.Typeable (eqT, (:~:) (..))
import Ondim.MultiWalk.Basic
import {-# SOURCE #-} Ondim.MultiWalk.Combinators

-- * Class

-- * CanLift class

class CanLift (s :: Type) where
  liftSub ::
    Monad m =>
    Carrier s ->
    Ondim m (Carrier s)

-- * Substructure class

class Substructure (target :: Type) (needle :: Type) where
  getSub :: Carrier needle -> [target]

{- | This overlappable instance is necessary because the HasSub machinery tries to
   match the target with all the fields, including non-matching fields.
-}
instance {-# OVERLAPPABLE #-} Substructure target needle where
  getSub = mempty

-- * OndimNode class

class
  ( HasSub GSubTag (ExpTypes t) t,
    AllMods CanLift (ExpTypes t),
    CanLift (NodeListSpec t),
    Carrier (NodeListSpec t) ~ [t],
    OndimCast t
  ) =>
  OndimNode t
  where
  type ExpTypes t :: Spec
  type NodeListSpec t :: Type
  type NodeListSpec t = NLDef t
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

instance OndimNode a => OndimNode [a] where
  type ExpTypes [a] = 'SpecSelf (NodeList a)
  type NodeListSpec [a] = Trav [] (NodeList a)
  castFrom p = (one .) <$> castFrom p
  renderNode = foldMap' <$> renderNode
  nodeAsText = foldMap' <$> nodeAsText

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
