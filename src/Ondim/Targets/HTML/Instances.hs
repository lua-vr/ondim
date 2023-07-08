{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.HTML.Instances where

import Data.Char (isSpace)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Lucid qualified as T
import Lucid.Base qualified as L
import Lucid.Html5 qualified as L
import Ondim
import Text.XML qualified as X

newtype HtmlDocument = HtmlDocument {documentRoot :: HtmlElement}
  deriving (Eq, Show, Generic)

toHtmlDocument :: X.Document -> HtmlDocument
toHtmlDocument = HtmlDocument . toHtmlElement . X.documentRoot

instance L.ToHtml HtmlDocument where
  toHtml (HtmlDocument el) = L.doctype_ <> L.toHtml el
  toHtmlRaw = mempty

instance OndimNode HtmlDocument where
  type ExpTypes HtmlDocument = '[ToSpec (Nesting HtmlElement)]
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Rendered = Just $ one . L.renderBS . L.toHtml
    | otherwise = Nothing

{- | We use a new XML datatype so that we can group the node with the newline space
  before it. This makes the output formatting much better.
-}
data HtmlElement = HtmlElement
  { preNewline :: Bool,
    elementTag :: Text,
    elementAttrs :: [Attribute],
    elementChildren :: ![HtmlNode]
  }
  deriving (Eq, Show, Generic)

toHtmlElement :: X.Element -> HtmlElement
toHtmlElement (X.Element name attrs nodes) =
  HtmlElement False (X.nameLocalName name) (map (first X.nameLocalName) $ Map.toList attrs) $
    toHtmlNodes nodes

voidElems :: Set.Set Text
voidElems = Set.fromAscList $ T.words "area base br col command embed hr img input keygen link meta param source track wbr"

instance L.ToHtml HtmlElement where
  toHtml (HtmlElement nl name attrs child)
    | nl = "\n" <> L.with elm attrs'
    | otherwise = L.with elm attrs'
    where
      attrs' = map (uncurry L.makeAttribute) attrs
      childHtml =
        if name == "script" || name == "style"
          then L.toHtmlRaw child
          else L.toHtml child
      elm =
        if name `Set.member` voidElems
          then L.makeElementNoEnd name
          else L.makeElement name childHtml
  toHtmlRaw = mempty

instance OndimNode HtmlElement where
  type ExpTypes HtmlElement = '[ToSpec Attribute, ToSpec HtmlNode]
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Rendered = Just $ one . L.renderBS . L.toHtml
    | otherwise = Nothing

data HtmlNode
  = Element HtmlElement
  | TextNode Text
  | RawNode Text
  deriving (Eq, Show, Generic)

instance L.ToHtml HtmlNode where
  toHtml (Element el) = L.toHtml el
  toHtml (TextNode t) = L.toHtml t
  toHtml (RawNode t) = L.toHtmlRaw t
  toHtmlRaw Element {} = mempty
  toHtmlRaw (TextNode t) = T.toHtmlRaw t
  toHtmlRaw (RawNode t) = T.toHtmlRaw t

instance L.ToHtml [HtmlNode] where
  toHtml = foldMap' L.toHtml
  toHtmlRaw = foldMap' L.toHtmlRaw

-- | Convert from XML nodes to @HtmlNode@
toHtmlNodes :: [X.Node] -> [HtmlNode]
toHtmlNodes = foldr go [] . filter notEmpty
  where
    notEmpty (X.NodeContent "") = False
    notEmpty _ = True

    go (X.NodeContent t) []
      | T.all isSpace t, T.any ('\n' ==) t = []
    go (X.NodeContent t) (Element el : xs)
      | T.all isSpace t, T.any ('\n' ==) t = Element el {preNewline = True} : xs
    go (X.NodeContent t) (TextNode t' : xs) = TextNode (t <> t') : xs
    go (X.NodeContent t) l = TextNode t : l
    go (X.NodeElement el) xs  = Element (toHtmlElement el) : xs
    go X.NodeComment {} xs = xs
    go X.NodeInstruction {} xs = xs

instance OndimNode HtmlNode where
  type ExpTypes HtmlNode = '[ToSpec (Nesting HtmlElement), ToSpec (OneSub Text)]
  identify (Element (HtmlElement _ name _ _)) = T.stripPrefix "e:" name
  identify _ = Nothing
  children = specChildren
  attributes = specAttributes
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . TextNode
    | Just Refl <- eqT @t @HtmlDocument = Just $ elementChildren . documentRoot
    | otherwise = Nothing
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . toStrict . L.renderText . L.toHtml
    | Just Refl <- eqT @t @Rendered = Just $ one . L.renderBS . L.toHtml
    | otherwise = Nothing

rawNode :: Text -> HtmlNode
rawNode = RawNode
