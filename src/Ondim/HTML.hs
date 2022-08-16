-- | HTML implementation.

module Ondim.HTML where
import Ondim
import Text.XmlHtml qualified as X
import Ondim.Extra
import Data.Char (isSpace)
import Data.Map.Syntax ((##))
import qualified Data.Text as T

{- | Ondim HTML tag. (Used for instances).
-}
data HtmlTag (m :: Type -> Type)

{- | We use a new XML datatype so that we can group the node with the newline space
   before it. This makes the output formatting much better.
-}
data HtmlNode
  = Element {preNewline :: Bool, elementTag :: Text, elementAttrs :: [(Text, Text)], elementChildren :: [HtmlNode]}
  | TextNode Text
  deriving (Eq, Show, Generic)

{- | Convert from XmlHtml nodes to @HtmlNode@
-}
fromNodeList :: [X.Node] -> [HtmlNode]
fromNodeList = foldr go [] . filter notEmpty
  where
    notEmpty (X.TextNode "") = False
    notEmpty X.Comment {} = False
    notEmpty _ = True

    go (X.TextNode t) (el@Element{} : xs)
      | T.all isSpace t, T.any ('\n' ==) t =
          el { preNewline = True } : xs
    go (X.TextNode t) (TextNode t' : xs) = TextNode (t <> t') : xs
    go (X.TextNode t) l = TextNode t : l
    go (X.Element x y z) xs = Element False x y (fromNodeList z) : xs
    go X.Comment{} xs = xs

{- | Convert from @HtmlNode@ nodes to XmlHtml
-}
toNodeList :: [HtmlNode] -> [X.Node]
toNodeList = foldMap go
  where go (Element n a b c)
          | n = [X.TextNode "\n", X.Element a b (toNodeList c)]
          | otherwise = [X.Element a b (toNodeList c)]
        go (TextNode t) = [X.TextNode t]

{- | Concatenates all text inside the node.
-}
nodeText :: HtmlNode -> Text
nodeText (TextNode t) = t
nodeText el@Element{} = foldMap nodeText (elementChildren el)

instance Monad m => OndimTag (HtmlTag m) where
  type OndimTypes (HtmlTag m) = '[HtmlNode, Attribute, ExpansibleText]
  type OndimMonad (HtmlTag m) = m

instance Monad m => OndimNode (HtmlTag m) HtmlNode where
  type ExpTypes HtmlNode = '[Attribute, HtmlNode]
  identify (Element _ name _ _) = Just name
  identify _ = Nothing
  fromText = Just (one . TextNode)
  validIdentifiers = Just validHtmlTags

instance HasSub (HtmlTag m) HtmlNode Attribute
instance HasSub (HtmlTag m) HtmlNode HtmlNode

instance Monad m => OndimNode (HtmlTag m) ExpansibleText where
  type ExpTypes ExpansibleText = '[]

instance Monad m => OndimNode (HtmlTag m) Attribute where
  type ExpTypes Attribute = '[ExpansibleText]
  identify (t,_) = Just t

instance HasSub (HtmlTag m) Attribute ExpansibleText where
  getSubs (_,t) = [t]
  setSubs (k,_) t = (k, mconcat t)

-- | A hack, unfortunately.
rawNode :: Text -> HtmlNode
rawNode txt = Element False "to-be-removed" [("xmlhtmlRaw", "")] [TextNode txt]

bindDefaults :: forall m t. Monad m =>
  Ondim (HtmlTag m) t -> Ondim (HtmlTag m) t
bindDefaults st = st
 `binding` do
   "ignore" ## ignore @HtmlNode
   "if-bound" ## ifBound
   "switch" ## switchBound
   "bind" ## bind
   "bind-text" ## bindText nodeText
  `bindingFilters` do
    "attrSub" ## attrSub

-- * Template loading helpers

fromDocument :: Monad m => X.Document -> Expansion (HtmlTag m) HtmlNode
fromDocument = fromTemplate . fromNodeList . X.docContent

expandDocument :: Monad m => X.Document -> Ondim (HtmlTag m) X.Document
expandDocument doc = do
  nodes <- liftNodes (fromNodeList $ X.docContent doc)
  pure $ doc { X.docContent = toNodeList nodes }

-- * Valid html tags

{-
  Array.from(document.querySelectorAll('tr > td:first-child > a > code'))
       .map(e => e.textContent.slice(1,-1))
       .join('\n')

-}
{- | Valid HTML5 tags, scraped from
   <https://developer.mozilla.org/en-US/docs/Web/HTML/Element>.
-}
validHtmlTags :: [Text]
validHtmlTags =
  [ "html"
  , "base"
  , "head"
  , "link"
  , "meta"
  , "style"
  , "title"
  , "body"
  , "address"
  , "article"
  , "aside"
  , "footer"
  , "header"
  , "h1"
  , "h2"
  , "h3"
  , "h4"
  , "h5"
  , "h6"
  , "main"
  , "nav"
  , "section"
  , "blockquote"
  , "dd"
  , "div"
  , "dl"
  , "dt"
  , "figcaption"
  , "figure"
  , "hr"
  , "li"
  , "menu"
  , "ol"
  , "p"
  , "pre"
  , "ul"
  , "a"
  , "abbr"
  , "b"
  , "bdi"
  , "bdo"
  , "br"
  , "cite"
  , "code"
  , "data"
  , "dfn"
  , "em"
  , "i"
  , "kbd"
  , "mark"
  , "q"
  , "rp"
  , "rt"
  , "ruby"
  , "s"
  , "samp"
  , "small"
  , "span"
  , "strong"
  , "sub"
  , "sup"
  , "time"
  , "u"
  , "var"
  , "wbr"
  , "area"
  , "audio"
  , "img"
  , "map"
  , "track"
  , "video"
  , "embed"
  , "iframe"
  , "object"
  , "picture"
  , "portal"
  , "source"
  , "svg"
  , "canvas"
  , "noscript"
  , "script"
  , "del"
  , "ins"
  , "caption"
  , "col"
  , "colgroup"
  , "table"
  , "tbody"
  , "td"
  , "tfoot"
  , "th"
  , "thead"
  , "tr"
  , "button"
  , "datalist"
  , "fieldset"
  , "form"
  , "input"
  , "label"
  , "legend"
  , "meter"
  , "optgroup"
  , "option"
  , "output"
  , "progress"
  , "select"
  , "textarea"
  , "details"
  , "dialog"
  , "summary"
  , "slot"
  , "template"
  , "acronym"
  , "applet"
  , "bgsound"
  , "big"
  , "blink"
  , "center"
  , "content"
  , "dir"
  , "font"
  , "frame"
  , "frameset"
  , "hgroup"
  , "image"
  , "keygen"
  , "marquee"
  , "menuitem"
  , "nobr"
  , "noembed"
  , "noframes"
  , "param"
  , "plaintext"
  , "rb"
  , "rtc"
  , "shadow"
  , "spacer"
  , "strike"
  , "tt"
  , "xmp"
  ]
