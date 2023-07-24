{-# LANGUAGE UndecidableInstances #-}

module Ondim.Targets.Whiskers.Instances where

import Data.Typeable (eqT, (:~:) (..))
import Ondim

data Node
  = Section Text [Attribute] [Node]
  | Single Text [Attribute]
  | Textual Text
  deriving (Eq, Ord, Show, Generic, NFData)

renderWhiskers :: [Node] -> Text
renderWhiskers = foldMap go
  where
    go = \case
      Section _ _ n -> foldMap go n
      Textual t -> t
      Single {} -> mempty

instance OndimNode Node where
  type
    ExpTypes Node =
      'SpecList
        '[ ToSpec (NL Node)
         ]
  children = specChildren
  attributes = return . getSubstructure
  identify = \case
    Section t _ _ -> Just t
    Single t _ -> Just t
    _ -> Nothing
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . Textual
    | otherwise = Nothing
  renderNode = Just $ encodeUtf8 . renderWhiskers . one
  nodeAsText = Just $ renderWhiskers . one
