module Ondim.Targets.HTML.Instances where

import Data.Bitraversable (bimapM)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Lucid qualified as T
import Lucid.Base qualified as L
import Lucid.Html5 qualified as L
import Ondim
import Ondim.Advanced
import Ondim.Targets.Whiskers (WAttribute, WNode (Textual), parseWhiskers, renderWhiskers)
import Text.XML qualified as X
import Data.Char (isSpace)
import Data.Foldable (foldrM)

newtype HtmlDocument = HtmlDocument {documentRoot :: HtmlElement}
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

toHtmlDocument :: X.Document -> Either String HtmlDocument
toHtmlDocument = fmap HtmlDocument . toHtmlElement . X.documentRoot

instance L.ToHtml HtmlDocument where
  toHtml (HtmlDocument el) = L.doctype_ <> L.toHtml el
  toHtmlRaw = mempty

instance Expansible HtmlDocument where
  expandSubs (HtmlDocument e) = HtmlDocument <$> expandSubs e

instance OndimNode HtmlDocument where
  renderNode = Just $ L.renderBS . L.toHtml

data HtmlElement = HtmlElement
  { preNewline :: !Bool,
    elementTag :: ![WNode],
    elementAttrs :: ![WAttribute],
    elementChildren :: ![HtmlNode]
  }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Expansible HtmlElement where
  expandSubs (HtmlElement nl t a c) = HtmlElement nl t <$> expandSubs a <*> expandSubs c

-- | Convert from XML nodes to @HtmlNode@
toHtmlNodes :: [X.Node] -> Either String [HtmlNode]
toHtmlNodes = foldrM go [] . filter notEmpty
  where
    notEmpty (X.NodeContent "") = False
    notEmpty _ = True
  
    go (X.NodeContent t) []
      | T.all isSpace t, T.any ('\n' ==) t = return []
    go (X.NodeContent t) (Element el : xs)
      | T.all isSpace t, T.any ('\n' ==) t = return $ Element el {preNewline = True} : xs
    go (X.NodeContent t) (TextNode t' : xs) = do
      p <- parse t
      return $ TextNode (p <> t') : xs
    go (X.NodeContent t) l = do
      p <- parse t
      return $ TextNode p : l
    go (X.NodeElement el) xs = do
      el' <- toHtmlElement el
      return $ Element el' : xs
    go X.NodeComment {} xs = return xs
    go X.NodeInstruction {} xs = return xs
    parse = parseWhiskers ("${", "}") ""

toHtmlElement :: X.Element -> Either String HtmlElement
toHtmlElement (X.Element name attrs nodes) =
  HtmlElement False
    <$> parse (X.nameLocalName name)
    <*> mapM (bimapM (return . X.nameLocalName) parse) (Map.toList attrs)
    <*> toHtmlNodes nodes
  where
    parse = parseWhiskers ("${", "}") ""

voidElems :: Set.Set Text
voidElems = Set.fromAscList $ T.words "area base br col command embed hr img input keygen link meta param source track wbr"

instance L.ToHtml HtmlElement where
  toHtml (HtmlElement nl (renderWhiskers -> name) attrs child)
    | nl = "\n" <> L.with elm attrs'
    | otherwise = L.with elm attrs'
    where
      attrs' =
        map (uncurry L.makeAttribute . fmap renderWhiskers) attrs
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
  renderNode = Just $ L.renderBS . L.toHtml

data HtmlNode
  = Element !HtmlElement
  | TextNode ![WNode]
  | RawNode !Text
  deriving (Eq, Ord, Show, Generic, NFData)

instance L.ToHtml HtmlNode where
  toHtml (Element el) = L.toHtml el
  toHtml (TextNode t) = L.toHtml $ renderWhiskers t
  toHtml (RawNode t) = L.toHtmlRaw t
  toHtmlRaw Element {} = mempty
  toHtmlRaw (TextNode t) = T.toHtmlRaw $ renderWhiskers t
  toHtmlRaw (RawNode t) = T.toHtmlRaw t

instance L.ToHtml [HtmlNode] where
  toHtml = foldMap' L.toHtml
  toHtmlRaw = foldMap' L.toHtmlRaw

instance Expansible HtmlNode where
  expandSubs = \case
    Element e -> Element <$> expandSubs e
    TextNode t -> TextNode <$> expandSubs t
    n@RawNode {} -> return n

instance OndimNode HtmlNode where
  identify (Element (HtmlElement _ (renderWhiskers -> name) _ _)) =
    T.stripPrefix "e:" name
  identify _ = Nothing
  children = \case
    Element (HtmlElement {elementChildren = c}) -> c
    _ -> []
  attributes = \case
    Element (HtmlElement {elementAttrs = attrs}) -> do
      forM attrs \(k, v) -> do
        v' <- expandSubs v
        return (k, renderWhiskers v')
    _ -> return []
  castFrom :: forall t. (Typeable t) => Maybe (t -> [HtmlNode])
  castFrom
    | Just Refl <- eqT @t @Text = Just $ one . TextNode . one . Textual
    | Just Refl <- eqT @t @HtmlDocument = Just $ elementChildren . documentRoot
    | otherwise = Nothing
  nodeAsText = Just $ toStrict . L.renderText . L.toHtml
  renderNode = Just $ L.renderBS . L.toHtml
