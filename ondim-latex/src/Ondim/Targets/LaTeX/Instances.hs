module Ondim.Targets.LaTeX.Instances
  ( LaTeXNode (..)
  , LaTeXDoc (..)
  , escapeLaTeX
  ) where

import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Ondim
import Ondim.Advanced
import Data.Bitraversable (bimapM)

expandLNodes :: forall s. [LaTeXNode] -> Ondim s [LaTeXNode]
expandLNodes = foldr go (pure mempty)
  where
    go i@(Command "sep" _ _) x = (i :) <$> x
    go i x = liftA2 (++?) (expandNode i) x
    xs ++? ys = foldr cons ys xs
    cons (Command "sep" _ _) (Command "sep" _ _ : xs) = Text "\n\n" : xs
    cons x xs = x : xs

type LAttribute = ([LaTeXNode], [LaTeXNode])

instance Expansible LAttribute where
  expandSubs = bimapM expandLNodes expandLNodes

data LaTeXNode
  = Command !Text ![LAttribute] ![LaTeXNode]
  | Text !Text
  | Comment !Text
  deriving (Eq, Ord, Show, Read, Generic, NFData)

renderLaTeX :: Foldable t => t LaTeXNode -> Text
renderLaTeX = foldMap go
  where
    go :: LaTeXNode -> Text
    go (Command _ _ n) = renderLaTeX n
    go (Text t) = t
    go (Comment t) = "%" <> t

escapeLaTeX :: Text -> Text
escapeLaTeX = T.concatMap \case
  '#' -> "\\#"
  '$' -> "\\$"
  '%' -> "\\%"
  '&' -> "\\&"
  '_' -> "\\_"
  '{' -> "\\{"
  '}' -> "\\}"
  '\\' -> "\\textbackslash{}"
  '^' -> "\\textasciicircum{}"
  '~' -> "\\textasciitilde{}"
  c -> one c

instance Expansible LaTeXNode where
  expandSubs = \case
    Command t a n -> Command t <$> expandSubs a <*> expandLNodes n
    t -> return t

instance OndimNode LaTeXNode where
  children = \case
    Command _ _ c -> toList c
    _ -> []
  attributes (Command _ a _) =
    bimap renderLaTeX renderLaTeX <<$>> expandSubs a
  attributes _ = pure []
  identify = \case
    Command t _ _ -> Just t
    Text {} -> Nothing
    Comment {} -> Nothing
  castFrom :: forall t. (Typeable t) => Maybe (t -> [LaTeXNode])
  castFrom
    | Just Refl <- eqT @t @LaTeXDoc = Just (toList . documentNodes)
    | Just Refl <- eqT @t @Text = Just $ one . Text
    | otherwise = Nothing

newtype LaTeXDoc = LaTeXDoc {documentNodes :: [LaTeXNode]}
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

instance Expansible LaTeXDoc where
  expandSubs = fmap LaTeXDoc . expandLNodes . documentNodes

instance OndimNode LaTeXDoc where
  renderNode = Just (encodeUtf8 . renderLaTeX . documentNodes)
