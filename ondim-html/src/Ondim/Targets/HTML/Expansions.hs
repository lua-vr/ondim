module Ondim.Targets.HTML.Expansions where

import Ondim
import Ondim.Extra.Expansions
import Ondim.Extra.Standard (standardMap)
import Ondim.Targets.HTML.Instances
import Ondim.Targets.HTML.Parser (parseT)
import Text.XML qualified as X
import Ondim.Debug (throwTemplateError)

defaultState :: OndimState s
defaultState =
  OndimState
    { expansions = exps
    }
  where
    exps = mapToNamespace do
      standardMap
      "raw.html" ## \(node :: HtmlNode) -> do
        t <- lookupAttr' "text" node
        return [RawNode t]
      "expanded.html" ## \(node :: HtmlNode) -> do
        t <- parseT <$> lookupAttr' "text" node
        result <- case toHtmlNodes $ X.elementNodes $ X.documentRoot t of
          Right nodes -> return nodes
          Left err -> throwTemplateError (toText err)
        expandSubs result
