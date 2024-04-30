module Ondim.Targets.Whiskers.Instances where

import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Ondim
import Ondim.Advanced
import Ondim.Debug

type WAttribute = (Text, [WNode])

instance Expansible [WAttribute] where
  expandSubs = foldMapM go
    where
      go (k, v)
        | Just (k', '?') <- T.unsnoc k =
            expand k' v
              `catchFailure` \_ _ _ _ -> return []
        | otherwise = expand k v
      expand k v
        | Just name <- T.stripPrefix "e:" k =
            map (second (one . Textual)) <$> callTemplate name
        | otherwise = one . (k,) <$> expandSubs v

data WNode
  = Section !Text ![WAttribute] ![WNode]
  | Single !Text ![WAttribute]
  | Textual !Text
  deriving (Eq, Ord, Show, Generic, NFData)

renderWhiskers :: (Foldable t) => t WNode -> Text
renderWhiskers = foldMap go
  where
    go = \case
      Section _ _ n -> foldMap go n
      Textual t -> t
      Single {} -> mempty

instance Expansible WNode where
  expandSubs = \case
    Section t a n -> Section t <$> expandSubs a <*> expandSubs n
    Single t a -> Single t <$> expandSubs a
    t@Textual {} -> return t

instance OndimNode WNode where
  children = \case
    Section _ _ w -> w
    _ -> []
  attributes = \case
    Section _ a _ -> f a
    Single _ a -> f a
    Textual {} -> pure []
    where
      f a = fmap renderWhiskers <<$>> expandSubs a
  identify = \case
    Section t _ _ -> Just t
    Single t _ -> Just t
    _ -> Nothing
  castFrom :: forall t. (OndimNode t) => Maybe (t -> [WNode])
  castFrom
    | Just Refl <- eqT @t @Text = Just $ one . Textual
    | Just cast <- nodeAsText = Just $ one . Textual . cast
    | otherwise = Nothing
