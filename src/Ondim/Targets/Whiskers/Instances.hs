{-# LANGUAGE UndecidableInstances #-}

module Ondim.Targets.Whiskers.Instances where

import Ondim
import Data.Typeable ((:~:)(..), eqT)

data Node
  = Section Text [Attribute] [Node]
  | Single Text [Attribute]
  | Textual Text
  deriving (Eq, Ord, Show, Generic)

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
      '[ ToSpec Attribute,
         ToSpec Node
       ]
  children = specChildren
  attributes = specAttributes
  identify = \case
    Section t _ _ -> Just t
    Single t _ -> Just t
    _ -> Nothing
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . Textual
    | otherwise = Nothing
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . renderWhiskers . one
    | Just Refl <- eqT @t @Rendered = Just $ one . RenderedText . renderWhiskers . one
    | otherwise = Nothing
