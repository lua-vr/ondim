-- | Examples of expansions.

module Ondim.Extra where
import Ondim
import Data.Tree
import Relude.Extra.Map
import Data.Map.Syntax

class Textfiable t where
  textify :: t -> Maybe Text

instance Textfiable t => Textfiable (Tree t) where
  textify = textify . rootLabel

instance Textfiable t => Textfiable [t] where
  textify = foldMap textify


-- The examples

silence :: Monad m => a -> OndimT t m [b]
silence = const $ pure []

ignore :: (Monad m, OndimNode t) => Expansions' m t
ignore = "ignore" ## silence

ifElse :: (Monad m, OndimNode t) => Bool -> Expansion m t
ifElse cond oel = do
  els <- children <$> oel
  let (ifEls, drop 1 -> elseEls) =
        break ((Just "else" ==) . identify . rootLabel) els
  if cond
    then pure ifEls
    else pure elseEls

ifElseBound :: (Monad m, OndimNode t) => Expansions' m t
ifElseBound = "if-bound" ## ifBoundExpansion

switchCases :: (Monad m, OndimNode t) => Text -> Expansions' m t
switchCases tag = do
  "case" ## \caseNode -> do
    node'@(children -> els') <- caseNode
    let attrs = attributes node'
    if tag `member` attrs || Just tag == lookup "tag" attrs
    then pure els'
    else pure []

switch :: (Monad m, OndimNode t) =>
  Text -> Expansion m t
switch tag = runChildrenWith' (switchCases tag)

switchWithDefault :: forall m t.
  Monad m =>
  (OndimNode t) =>
  Text -> Expansion m t
switchWithDefault tag oel = do
  els <- children <$> oel
  pure $ fromMaybe [] $ fmap subForest $
    find (\n@(Node x _) -> nameIs "case" x && hasTag n) els <|>
    find (\  (Node x _) -> nameIs "default" x) els
  where
    nameIs n x = identify x == Just n
    hasTag (attributes -> attrs) = tag `member` attrs || Just tag == lookup "tag" attrs

switchBound :: Monad m =>
  (OndimNode t, Textfiable t, HasEmpty t) =>
  Expansions' m t
switchBound = "switch" ## switchExpansion


-- Implementations

ifBoundExpansion :: (Monad m, OndimNode t) => Expansion m t
ifBoundExpansion oel = do
  node <- oel
  let attrs = attributes node
  bound <- case lookup "tag" attrs of
    Just tag -> asksE (member (fromText tag))
    Nothing -> pure False
  ifElse bound (pure node)

switchExpansion :: forall m t.
  Monad m =>
  (OndimNode t, Textfiable t, HasEmpty t) =>
  Expansion m t
switchExpansion oel = do
  node@(children -> els) <- oel
  let attrs = attributes node
  fromMaybe els <$> runMaybeT do
    tag <- hoistMaybe $ lookup "tag" attrs
    tagE <- hoistMaybe =<< lift (asksE (lookup (fromText tag)))
    tagC <- hoistMaybe =<< lift (textify <$> tagE (pure emptyNode))
    lift $ switchWithDefault tagC oel
