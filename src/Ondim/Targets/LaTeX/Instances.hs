{-# LANGUAGE UndecidableInstances #-}

module Ondim.Targets.LaTeX.Instances where

import Control.Monad (liftM2)
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Ondim

data Node
  = Command Text [([Node], [Node])] [Node]
  | Text Text
  | Comment Text
  deriving (Eq, Ord, Show, Generic)

renderLaTeX :: [Node] -> Text
renderLaTeX = foldMap go
  where
    go = \case
      Command _ _ n -> foldMap go n
      Text t -> t
      Comment t -> "%" <> t

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

instance OndimNode Node where
  type
    ExpTypes Node =
      'SpecList
        '[ ToSpec (Trav [] Node),
           ToSpec Node
         ]
  children = specChildren
  attributes (Command _ pairs _) =
    forM pairs \(k, v) ->
      liftM2
        (,)
        (renderLaTeX <$> liftNodes k)
        (renderLaTeX <$> liftNodes v)
  attributes _ = pure []
  identify = \case
    Command t _ _ -> Just t
    _ -> Nothing
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . Text
    | otherwise = Nothing
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . renderLaTeX . one
    | Just Refl <- eqT @t @Rendered = Just $ one . encodeUtf8 . renderLaTeX . one
    | otherwise = Nothing
