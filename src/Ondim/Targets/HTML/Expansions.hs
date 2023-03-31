module Ondim.Targets.HTML.Expansions where

import Ondim
import Ondim.Extra.Expansions
import Ondim.Targets.HTML.Instances

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim HtmlTag m t ->
  Ondim HtmlTag m t
bindDefaults st =
  st
    `binding` do
      "o" #. do
        "ignore" #* ignore
        "if" #* ifBound
        "match" #* switchBound
        "bind" #* bind
        "scope" #* scope
        "bind-text" ## bindText nodeText
        "with" #* with
        "open" #* open
        "debug" #* debug
      "@try" ## const $ pure ([] :: [Attribute])
    `bindingFilters` do
      "attrSub" $# attrSub
      "mbAttr" $# mbAttrFilter
      "notBound" $# notBoundFilter @HtmlNode validHtmlTags

-- * Valid html tags

{-
  Array.from(document.querySelectorAll('tr > td:first-child > a > code'))
       .map(e => e.textContent.slice(1,-1))
       .join('\n')

-}

{- | Valid HTML5 tags, scraped from
   <https://developer.mozilla.org/en-US/docs/Web/HTML/Element>.
-}
validHtmlTags :: Set Text
validHtmlTags =
  fromList
    [ "to-be-removed", -- TODO: this is a hack
      "html",
      "base",
      "head",
      "link",
      "meta",
      "style",
      "title",
      "body",
      "address",
      "article",
      "aside",
      "footer",
      "header",
      "h1",
      "h2",
      "h3",
      "h4",
      "h5",
      "h6",
      "main",
      "nav",
      "section",
      "blockquote",
      "dd",
      "div",
      "dl",
      "dt",
      "figcaption",
      "figure",
      "hr",
      "li",
      "menu",
      "ol",
      "p",
      "pre",
      "ul",
      "a",
      "abbr",
      "b",
      "bdi",
      "bdo",
      "br",
      "cite",
      "code",
      "data",
      "dfn",
      "em",
      "i",
      "kbd",
      "mark",
      "q",
      "rp",
      "rt",
      "ruby",
      "s",
      "samp",
      "small",
      "span",
      "strong",
      "sub",
      "sup",
      "time",
      "u",
      "var",
      "wbr",
      "area",
      "audio",
      "img",
      "map",
      "track",
      "video",
      "embed",
      "iframe",
      "object",
      "picture",
      "portal",
      "source",
      "svg",
      "canvas",
      "noscript",
      "script",
      "del",
      "ins",
      "caption",
      "col",
      "colgroup",
      "table",
      "tbody",
      "td",
      "tfoot",
      "th",
      "thead",
      "tr",
      "button",
      "datalist",
      "fieldset",
      "form",
      "input",
      "label",
      "legend",
      "meter",
      "optgroup",
      "option",
      "output",
      "progress",
      "select",
      "textarea",
      "details",
      "dialog",
      "summary",
      "slot",
      "template",
      "acronym",
      "applet",
      "bgsound",
      "big",
      "blink",
      "center",
      "content",
      "dir",
      "font",
      "frame",
      "frameset",
      "hgroup",
      "image",
      "keygen",
      "marquee",
      "menuitem",
      "nobr",
      "noembed",
      "noframes",
      "param",
      "plaintext",
      "rb",
      "rtc",
      "shadow",
      "spacer",
      "strike",
      "tt",
      "xmp"
    ]
