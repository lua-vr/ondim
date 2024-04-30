{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Ondim.Internal.Class
  ( OndimNode (..),
    Expansible (..),
    ondimCast,
    Attribute,
  ) where

import {-# SOURCE #-} Ondim.Internal.Basic (Ondim)
import Data.Typeable ((:~:)(..), eqT)

-- * Attributes

-- | Alias for attributes
type Attribute = (Text, Text)

-- ** Class

-- * 'OndimNode' class

class Expansible (t :: Type) where
  -- | Expand only the substructures of a node.
  expandSubs :: t -> Ondim s t

class (Typeable t, Expansible t) => OndimNode t where
  -- | Returns the name of the node as defined by the 'OndimNode' instance.
  identify :: t -> Maybe Text
  identify _ = Nothing

  -- | Returns a list of attributes of the node as defined by the 'OndimNode' instance.
  attributes :: t -> Ondim s [Attribute]
  attributes _ = pure []

  -- | Returns the children of the node as defined by the 'OndimNode' instance.
  children :: t -> [t]
  children _ = []

  castFrom :: (OndimNode a) => Maybe (a -> [t])
  castFrom = Nothing

  -- | Converts the node to a 'LByteString' as defined by the 'OndimNode' instance.
  renderNode :: Maybe (t -> LByteString)
  renderNode = Nothing

  nodeAsText :: Maybe (t -> Text)
  nodeAsText = Nothing

instance {-# OVERLAPPABLE #-} (OndimNode a, Expansible (t a), Foldable t, Typeable t) => OndimNode (t a) where
  renderNode = foldMap' <$> renderNode
  nodeAsText = foldMap' <$> nodeAsText

-- Some data instances (won't lift)

instance (Expansible Text) where
  expandSubs = return
instance OndimNode Text where
  nodeAsText = Just id

instance (Expansible LByteString) where
  expandSubs = return
instance OndimNode LByteString

instance (Expansible Attribute) where
  expandSubs = return
instance OndimNode Attribute

ondimCast :: forall a b. (OndimNode a, OndimNode b) => Maybe (a -> [b])
ondimCast = castFrom <|> case eqT @a @[b] of
  Just Refl -> Just id
  Nothing -> Nothing
