{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module Ondim.HTML where
import Ondim
import Ondim.Extra
import Text.XmlHtml qualified as X
import Data.Tree
import Data.Attoparsec.Text
import Replace.Attoparsec.Text
import Data.Map.Syntax ((##), runMap, mapV)
import Relude.Extra.Map

data HTMLNode
  = Element Text
  | Attribute Text
  | AttributeValue Text -- To support attribute content expansion
  | Content Text
  | RawContent Text
  | Comment Text
  | Empty
  deriving (Eq, Ord, Show)

type OndimHtmlT m a = OndimT HTMLNode m a

data HtmlId -- Type for custom equality
  = Name Text
  | IsAttributeValue
  deriving (Eq, Ord, Show)

instance IsString HtmlId where
  fromString = Name . toText

instance OndimNode HTMLNode where
  type Identifier HTMLNode = HtmlId
  identify (Element name) = Just (fromText name)
  identify (Attribute name) = Just (fromText name)
  identify AttributeValue {} = Just IsAttributeValue
  identify Content {} = Nothing
  identify RawContent {} = Nothing
  identify Comment {} = Nothing
  identify Empty = Nothing
  children (Node _ c) = filter (p . rootLabel) c
    where p Element {} = True
          p Content {} = True
          p RawContent {} = True
          p Comment {} = True
          p _ = False
  attributes = fromList .
               mapMaybe (fmap unAttr . attributeFromTree) .
               subForest

instance HasEmpty HTMLNode where
  emptyValue = Empty

instance Textfiable HTMLNode where
  textify Element {} = Nothing
  textify Attribute {} = Nothing
  textify (AttributeValue t) = Just t
  textify (Content t) = Just t
  textify (RawContent t) = Just t
  textify Comment {} = Nothing
  textify Empty {} = Nothing

leaf :: a -> Tree a
leaf x = Node x []

newtype Attribute = Attr { unAttr :: (Text, Text) }

attributeToTree :: Attribute -> Tree HTMLNode
attributeToTree (Attr (name, value)) =
  Node (Attribute name) [leaf (AttributeValue value)]

attributeFromTree :: Tree HTMLNode -> Maybe Attribute
attributeFromTree (Node (Attribute name) values) = Just (Attr (name, value))
  where value = mconcat $ mapMaybe getValue values
        getValue (Node (AttributeValue v) _) = Just v
        getValue _ = Nothing
attributeFromTree _ = Nothing

nodeToTree :: X.Node -> Tree HTMLNode
nodeToTree (X.TextNode txt) = leaf (Content txt)
nodeToTree (X.Comment txt) = leaf (Comment txt)
nodeToTree (X.Element name attrs childs) =
  Node (Element name) $
    (attributeToTree . Attr <$> attrs) ++
    (nodeToTree <$> childs)

instance OndimRepr HTMLNode Attribute where
  toTree = attributeToTree
  fromTree = attributeFromTree

type AttrExpansion m = OndimHtmlT m Attribute -> OndimHtmlT m [Attribute]

type AttrExpansions m = ReprExpansions m HTMLNode Attribute

-- | A hack, unfortunately. I could not find a single HTML Haskell library
-- properly supporting raw content.
rawNode :: Text -> X.Node
rawNode txt = X.Element "TO-BE-REMOVED" [("xmlhtmlRaw", "")] [X.TextNode txt]

nodeFromTree :: Tree HTMLNode -> Maybe X.Node
nodeFromTree (Node (Content txt) _) = Just (X.TextNode txt)
nodeFromTree (Node (Comment txt) _) = Just (X.Comment txt)
nodeFromTree (Node (RawContent txt) _) = Just (rawNode txt)
nodeFromTree (Node (Element name) sub) = Just (X.Element name attrs childs)
  where
    attrs = mapMaybe (fmap unAttr . attributeFromTree) sub
    childs = mapMaybe nodeFromTree sub
nodeFromTree (Node (AttributeValue _) _) = Nothing
nodeFromTree (Node (Attribute _) _) = Nothing
nodeFromTree (Node Empty _) = Nothing

instance OndimRepr HTMLNode X.Node where
  toTree = nodeToTree
  fromTree = nodeFromTree

type NodeExpansion m = OndimHtmlT m X.Node -> OndimHtmlT m [X.Node]

type NodeExpansions m = ReprExpansions m HTMLNode X.Node

-- * Template loading helpers

fromDocument :: Monad m => X.Document -> Expansion m HTMLNode
fromDocument doc = fromTemplate $ X.docContent doc

expandDocument :: Monad m => X.Document -> OndimHtmlT m X.Document
expandDocument doc = do
  forest <- sequence . fmap fromTree <$>
            liftNodeForest (toTree <$> X.docContent doc)
  pure $ maybe doc (\c -> doc { X.docContent = c }) forest

-- * Substitution of ${name} in attribute text

newtype AttrValue = AttrValue Text

instance OndimRepr HTMLNode AttrValue where
  toTree (AttrValue t) = leaf (AttributeValue t)
  fromTree (Node (AttributeValue t) _) = Just (AttrValue t)
  fromTree _ = Nothing

interpParser :: Parser Text
interpParser = do
  _ <- string "${"
  s <- takeTill (== '}')
  _ <- char '}'
  pure s

interpEditor :: Monad m => Text -> OndimHtmlT m Text
interpEditor t = do
  fromMaybe t <$> runMaybeT do
    expansion <- hoistMaybe =<< lift (asksE (lookup (Name t)))
    content <- hoistMaybe =<< lift (textify <$> expansion (pure emptyNode))
    pure content

attrInterpolationExp :: Monad m => ReprExpansions m HTMLNode AttrValue
attrInterpolationExp =
  IsAttributeValue ## \attr -> do
    AttrValue t <- attr
    one . AttrValue <$>
      streamEditT interpParser interpEditor t

defaultExpansions :: Monad m => Expansions m HTMLNode
defaultExpansions =
  fromRight mempty $ runMap $ do
    mapV fromReprExpansion attrInterpolationExp
    ignore
    switchBound
    ifElseBound
