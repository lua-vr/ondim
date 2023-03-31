{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.HTML.Instances where

import Data.Char (isSpace)
import Data.Text qualified as T
import Ondim.MultiWalk.Combinators
  ( Conversible (convertFrom, convertTo),
    Converting,
    ToSpecList,
  )
import Ondim (OndimNode (..), OndimTag (..), Attribute)
import Text.XmlHtml qualified as X

-- | Ondim HTML tag. (Used for instances).
data HtmlTag

-- | We use a new XML datatype so that we can group the node with the newline space
--   before it. This makes the output formatting much better.
data HtmlNode
  = Element {preNewline :: Bool, elementTag :: Text, elementAttrs :: [(Text, Text)], elementChildren :: [HtmlNode]}
  | TextNode Text
  deriving (Eq, Show, Generic)

-- | Convert from XmlHtml nodes to @HtmlNode@
fromNodeList :: [X.Node] -> [HtmlNode]
fromNodeList = foldr go [] . filter notEmpty
  where
    notEmpty (X.TextNode "") = False
    notEmpty X.Comment {} = False
    notEmpty _ = True

    go (X.TextNode t) []
      | T.all isSpace t, T.any ('\n' ==) t = []
    go (X.TextNode t) (el@Element {} : xs)
      | T.all isSpace t,
        T.any ('\n' ==) t =
          el {preNewline = True} : xs
    go (X.TextNode t) (TextNode t' : xs) = TextNode (t <> t') : xs
    go (X.TextNode t) l = TextNode t : l
    go (X.Element x y z) xs = Element False x y (fromNodeList z) : xs
    go X.Comment {} xs = xs

-- | Convert from @HtmlNode@ nodes to XmlHtml
toNodeList :: [HtmlNode] -> [X.Node]
toNodeList = foldMap go
  where
    go (Element n a b c)
      | n = [X.TextNode "\n", X.Element a b (toNodeList c)]
      | otherwise = [X.Element a b (toNodeList c)]
    go (TextNode t) = [X.TextNode t]

-- | Concatenates all text inside the node.
nodeText :: HtmlNode -> Text
nodeText (TextNode t) = t
nodeText el@Element {} = foldMap nodeText (elementChildren el)

instance OndimTag HtmlTag where
  type OndimTypes HtmlTag = '[X.Document, HtmlNode, Attribute, Text]

deriving instance (Generic X.Document)

instance OndimNode HtmlTag X.Document where
  type ExpTypes X.Document = ToSpecList '[Converting [X.Node] HtmlNode]

instance Conversible HtmlTag [X.Node] [HtmlNode] where
  convertTo = fromNodeList
  convertFrom = toNodeList

instance OndimNode HtmlTag HtmlNode where
  type ExpTypes HtmlNode = ToSpecList '[Attribute, HtmlNode]
  identify (Element _ name _ _) = Just name
  identify _ = Nothing
  fromText = Just (one . TextNode)

-- | A hack, unfortunately.
rawNode :: Text -> HtmlNode
rawNode txt = Element False "to-be-removed" [("xmlhtmlRaw", "")] [TextNode txt]
