{-# LANGUAGE UndecidableInstances #-}

module Ondim.Targets.LaTeX.Instances where

import Control.Monad (liftM2)
import Data.Text qualified as T
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
      '[ ToSpec (Trav [] Node),
         ToSpec Node
       ]
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
  fromText = Just $ one . Text . escapeLaTeX
