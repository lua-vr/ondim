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
  deriving (Eq, Ord, Show, Generic, NFData)

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
        '[ ToSpec (Trav [] (NL Node)),
           ToSpec (NL Node)
         ]
  type NodeListSpec Node = NodeWithSep
  children = getSubstructure
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
  nodeAsText = Just $ renderLaTeX . one
  renderNode = (encodeUtf8 .) <$> nodeAsText

type NodeWithSep = Custom [Node] Void

instance CanLift NodeWithSep where
  liftSub = foldr go (pure [])
    where
      go i@(Command "sep" _ _) x = (i :) <$> x
      go i x = liftA2 (++?) (liftNode i) x

      xs ++? ys = foldr cons ys xs

      cons (Command "sep" _ _) (Command "sep" _ _ : xs) = Text "\n\n" : xs
      cons x xs = x : xs
