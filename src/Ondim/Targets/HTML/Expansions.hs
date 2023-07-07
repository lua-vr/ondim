module Ondim.Targets.HTML.Expansions where

import Data.ByteString.Builder (toLazyByteString)
import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter)
import Ondim.Extra.Expansions
import Ondim.Extra.Standard (attrSub, bindText, standardMap)
import Ondim.Targets.HTML.Instances
import Text.XmlHtml qualified as X

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
        t <- lookupAttr' "text" node
        fromMaybe (pure []) do
          parsed <- rightToMaybe $ X.parseHTML "" (encodeUtf8 t)
          return $ liftNodes $ fromNodeList $ X.docContent parsed
      "bind-text" ## bindText nodeText
    `bindingFilters` do
      "attrSub" $* attrSub '$' ('{', '}')
      "tryAttr" $# tryAttrFilter

expandHtml ::
  forall m a.
  HasCallStack =>
  GlobalConstraints m a =>
  Expansion m a
expandHtml node = do
  text <- lookupAttr' "text" node
  let parsed = X.parseHTML "expand.html input" $ encodeUtf8 text
  case parsed of
    Left e -> throwTemplateError $ toText e
    Right p
      | Just fT <- fromText ->
          liftNodes (fromNodeList $ X.docContent p)
            <&> toNodeList
            <&> X.renderHtmlFragment X.UTF8
            <&> toLazyByteString
            <&> decodeUtf8
            <&> fT
      | otherwise -> throwTemplateError "expand.html needs a fromText instance!"
