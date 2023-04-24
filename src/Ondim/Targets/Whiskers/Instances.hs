{-# LANGUAGE UndecidableInstances #-}

module Ondim.Targets.Whiskers.Instances where

import Ondim

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
  attributes = substructureAttributes
  identify = \case
    Section t _ _ -> Just t
    Single t _ -> Just t
    _ -> Nothing
  fromText = Just $ one . Textual
