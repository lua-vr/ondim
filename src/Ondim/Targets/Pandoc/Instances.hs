{-# LANGUAGE UndecidableInstances #-}

module Ondim.Targets.Pandoc.Instances where

import Data.Text qualified as T
import Ondim.Extra.Expansions
  ( Attribute,
    ExpansibleText,
    HasAttrs,
  )
import Ondim.MultiWalk.Combinators
import Ondim.MultiWalk.Core (OndimNode (..), OndimTag (..))
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition

getId :: [Text] -> Maybe Text
getId = asum . map (T.stripPrefix "e:")

data PandocTag

instance OndimTag PandocTag where
  type
    OndimTypes PandocTag =
      '[ Pandoc,
         Inline,
         Block,
         Attribute,
         ExpansibleText
       ]

instance OndimNode PandocTag Pandoc where
  type
    ExpTypes Pandoc =
      ToSpecList
        '[ Block
         ]

instance OndimNode PandocTag Block where
  type
    ExpTypes Block =
      ToSpecList
        '[ Inline,
           Under [Inline] Inline,
           Block,
           Under [Block] Block,
           Under ([Inline], [[Block]]) (Under [Inline] Inline),
           Under ([Inline], [[Block]]) (Under [[Block]] (Under [Block] Block)),
           Under Attr Attribute,
           OneSub ExpansibleText
         ]
  identify (Div (_, n, _) _) = getId n
  identify (Header _ (_, n, _) _) = getId n
  identify _ = Nothing
  validIdentifiers = Just []

instance HasAttrs PandocTag Block

instance OndimNode PandocTag Inline where
  type
    ExpTypes Inline =
      ToSpecList
        '[ Inline,
           Block,
           Converting Attr Attribute,
           OneSub ExpansibleText,
           Attribute
         ]
  identify (Span (_, n, _) _) = getId n
  identify _ = Nothing
  fromText = Just (toList . B.text)
  validIdentifiers = Just []

instance HasAttrs PandocTag Inline

instance Conversible PandocTag Attr [Attribute] where
  convertTo (x, y, z) =
    ("id", x)
      : ("class", T.intercalate " " (filter (not . T.isPrefixOf "e:") y))
      : z
  convertFrom = foldMap go
    where
      go ("id", a) = (a, [], [])
      go ("class", a) = ("", T.split (' ' ==) a, [])
      go x = ("", [], [x])

instance OndimNode PandocTag ExpansibleText where
  type ExpTypes ExpansibleText = '[]

instance OndimNode PandocTag Attribute where
  type ExpTypes Attribute = ToSpecList '[OneSub ExpansibleText]
  identify = Just . fst
