module Ondim.Targets.Aeson.Instances
  ( AesonNode (..)
  , nodeToValue
  , nodeFromValue
  , renderWhiskers
  )
  where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Bitraversable (bimapM)
import Data.List qualified as L
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Ondim
import Ondim.Advanced
import Ondim.Debug
import Ondim.Targets.Whiskers (parseWhiskers)
import Ondim.Targets.Whiskers.Instances (WNode (Textual), renderWhiskers)
import Relude.Extra.Map qualified as M

data AesonNode
  = Array' ![AesonNode]
  | Object' ![(Text, AesonNode)]
  | String' ![WNode]
  | Number' !Scientific
  | Bool' !Bool
  | Null'

nodeToValue :: AesonNode -> Value
nodeToValue = \case
  Array' l -> Array $ fromList $ map nodeToValue l
  Object' m -> Object $ KM.fromList $ map (bimap K.fromText nodeToValue) $ M.toPairs m
  String' t -> String (renderWhiskers t)
  Number' n -> Number n
  Bool' b -> Bool b
  Null' -> Null

nodeFromValue :: Value -> Either String AesonNode
nodeFromValue = \case
  Array l -> Array' <$> mapM nodeFromValue (toList l)
  Object m -> Object' <$> mapM (bimapM (pure . K.toText) nodeFromValue) (KM.toList m)
  String t -> String' <$> parseWhiskers ("${", "}") "" t
  Number n -> return $ Number' n
  Bool b -> return $ Bool' b
  Null -> return Null'

instance Expansible AesonNode where
  expandSubs = \case
    Array' a -> Array' <$> expandSubs a
    Object' m -> Object' <$> mapMaybeM go m
      where
        go (k, v)
          | Just (k', '?') <- T.unsnoc k =
              (Just . (k',) <$> expandSubs v)
                `catchFailure` \_ _ _ _ -> return Nothing
          | otherwise = Just . (k,) <$> expandSubs v
    String' t -> String' <$> expandSubs t
    x -> return x

instance OndimNode AesonNode where
  identify = \case
    (Object' o)
      | Just (String' name) <- L.lookup "$" o -> Just (renderWhiskers name)
    _ -> Nothing
  children = \case
    (Object' o)
      | Just (Array' a) <- L.lookup "$children" o -> a
    _ -> mempty
  attributes (Object' o) = mapMaybeM go $ M.toPairs o
    where
      go (k, v)
        | String' t <- v = do
            t' <- expandSubs t
            return $ Just (k, renderWhiskers t')
        | otherwise = return Nothing
  attributes _ = pure []
  castFrom :: forall t. (Typeable t) => Maybe (t -> [AesonNode])
  castFrom
    | Just Refl <- eqT @t @Text = Just $ one . String' . one . Textual
    | otherwise = Nothing
  nodeAsText = Just \case
    String' t -> renderWhiskers t
    _notString -> mempty
  renderNode = Just (encode . nodeToValue)
