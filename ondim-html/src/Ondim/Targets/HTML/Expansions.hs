module Ondim.Targets.HTML.Expansions where

import Ondim
import Ondim.Advanced
import Ondim.Extra.Expansions
import Ondim.Extra.Standard (standardMap)
import Ondim.Targets.HTML.Instances
import Ondim.Targets.HTML.Parser (parseT)
import Text.XML qualified as X

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
        return [rawNode t]
      "expanded.html" ## \(node :: HtmlNode) -> do
        t <- parseT <$> lookupAttr' "text" node
        expandSpec @(NL HtmlNode) $ toHtmlNodes $ X.elementNodes $ X.documentRoot t
