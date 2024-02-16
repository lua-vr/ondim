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
import Ondim.Advanced
import Ondim.Advanced.Substitution (SAttr, SAttrs, SText, SubstConfig (..), getSAttributes)
import Text.XML qualified as X

type HSConfig = 'SubstConfig '$' '{' '}'
type HtmlText = SText HSConfig
type HtmlAttr = SAttr HSConfig
type HtmlAttrs = SAttrs HSConfig

newtype HtmlDocument = HtmlDocument {documentRoot :: HtmlElement}
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

toHtmlDocument :: X.Document -> HtmlDocument
toHtmlDocument = HtmlDocument . toHtmlElement . X.documentRoot

instance L.ToHtml HtmlDocument where
  toHtml (HtmlDocument el) = L.doctype_ <> L.toHtml el
  toHtmlRaw = mempty

instance OndimNode HtmlDocument where
  type ExpTypes HtmlDocument = 'SpecList '[ToSpec (Nesting HtmlElement)]
  renderNode = Just $ L.renderBS . L.toHtml

{- | We use a new XML datatype so that we can group the node with the newline space
  before it. This makes the output formatting much better.
-}
data HtmlElement = HtmlElement
  { preNewline :: Bool,
    elementTag :: Text,
    elementAttrs :: [Attribute],
    elementChildren :: ![HtmlNode]
  }
  deriving (Eq, Ord, Show, Generic, NFData)

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
  type ExpTypes HtmlElement = 'SpecList '[ToSpec HtmlAttrs, ToSpec (NL HtmlNode)]
  renderNode = Just $ L.renderBS . L.toHtml

data HtmlNode
  = Element HtmlElement
  | TextNode Text
  | RawNode Text
  deriving (Eq, Ord, Show, Generic, NFData)

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
    go (X.NodeElement el) xs = Element (toHtmlElement el) : xs
    go X.NodeComment {} xs = xs
    go X.NodeInstruction {} xs = xs

instance OndimNode HtmlNode where
  type
    ExpTypes HtmlNode =
      'SpecList
        '[ ToSpec (Nesting HtmlElement),
           ToSpecSel ('ConsSel "TextNode") HtmlText
         ]
  identify (Element (HtmlElement _ name _ _)) = T.stripPrefix "e:" name
  identify _ = Nothing
  children = getSubstructure
  attributes = getSAttributes @HSConfig
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . TextNode
    | Just Refl <- eqT @t @HtmlDocument = Just $ elementChildren . documentRoot
    | otherwise = Nothing
  nodeAsText = Just $ toStrict . L.renderText . L.toHtml
  renderNode = Just $ L.renderBS . L.toHtml

rawNode :: Text -> HtmlNode
rawNode = RawNode
