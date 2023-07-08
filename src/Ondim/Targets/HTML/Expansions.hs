module Ondim.Targets.HTML.Expansions where

import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter)
import Ondim.Extra.Expansions
import Ondim.Extra.Standard (attrSub, standardMap)
import Ondim.Targets.HTML.Instances
import Text.HTML.DOM qualified as X
import qualified Text.XML as X

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      standardMap
      "raw" ## \(node :: HtmlNode) -> do
        t <- lookupAttr' "text" node
        return [rawNode t]
      "expanded" ## \(node :: HtmlNode) -> do
        t <- X.parseLT . toLText <$> lookupAttr' "text" node
        liftNodes $ toHtmlNodes $ X.elementNodes $ X.documentRoot t
    `bindingFilters` do
      "attrSub" $* attrSub '$' ('{', '}')
      "tryAttr" $# tryAttrFilter
