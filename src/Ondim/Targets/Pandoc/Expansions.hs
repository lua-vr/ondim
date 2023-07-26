module Ondim.Targets.Pandoc.Expansions where

import Ondim
import Ondim.Extra.Standard (standardMap)
import Ondim.Targets.Pandoc.Instances ()
import Text.Pandoc (Block (..), ListNumberDelim (..), ListNumberStyle (..))

defaultState :: Monad m => OndimState m
defaultState =
  OndimState
    { expansions = exps
    }
  where
    exps = mapToNamespace do
      standardMap
      "pandoc" #. do
        "block" #. do
          "ordered-list" ## \(node :: Block) -> do
            start :: Maybe Int <-
              (readMaybe . toString =<<) <$> lookupAttr "start" node
            style :: Maybe ListNumberStyle <-
              lookupAttr "style" node <&> chainedTo \case
                "default" -> Just DefaultStyle
                "example" -> Just Example
                "decimal" -> Just Decimal
                "lower-roman" -> Just LowerRoman
                "upper-roman" -> Just UpperRoman
                "lower-alpha" -> Just LowerAlpha
                "upper-alpha" -> Just UpperAlpha
                _ -> Nothing
            delim :: Maybe ListNumberDelim <-
              lookupAttr "delim" node <&> chainedTo \case
                "default" -> Just DefaultDelim
                "period" -> Just Period
                "one-paren" -> Just OneParen
                "two-parens" -> Just TwoParens
                _ -> Nothing
            nodes <- liftChildren node
            let attrs = (fromMaybe 1 start, fromMaybe DefaultStyle style, fromMaybe DefaultDelim delim)
                child = (`mapMaybe` nodes) \case
                  Div ("list.item", _, _) blks -> Just blks
                  _ -> Nothing
            return [OrderedList attrs child]
          "bullet-list" ## \(node :: Block) -> do
            nodes <- liftChildren node
            let child = (`mapMaybe` nodes) \case
                  Div ("list.item", _, _) blks -> Just blks
                  _ -> Nothing
            return [BulletList child]
          "definition-list" ## \(node :: Block) -> do
            nodes <- liftChildren node
            let child = (`mapMaybe` nodes) \case
                  Div
                    ("list.item", _, _)
                    [ Header _ ("list.item.term", _, _) ils,
                      Div ("list.item.definition", _, _) blks
                      ] -> Just (ils, [blks])
                  _ -> Nothing
            return [DefinitionList child]
