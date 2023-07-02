module Ondim.Targets.HTML.Expansions where

import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter, tryFilter)
import Ondim.Extra.Expansions
import Ondim.Targets.HTML.Instances
import qualified Text.XmlHtml as X
import Ondim.Extra.Standard (standardMap, bindText, attrSub)

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      "o" #. do
        standardMap
        "raw" ## \(node :: HtmlNode) -> do
          t <- lookupAttr' "text" node
          return [rawNode t]
        "expanded" ## \(node :: HtmlNode) -> do
          t <- lookupAttr' "text" node
          fromMaybe (pure []) do
            parsed <- rightToMaybe $ X.parseHTML "" (encodeUtf8 t)
            return $ liftNodes $ fromNodeList $ X.docContent parsed
        "bind-text" ## bindText nodeText
      "@try" ## const $ pure ([] :: [Attribute])
    `bindingFilters` do
      "attrSub" $* attrSub
      "tryAttr" $# tryAttrFilter
      "try" $# tryFilter @HtmlNode
