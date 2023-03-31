{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Pandoc.Instances where

import Data.Text qualified as T
import Ondim.MultiWalk.Combinators
import Ondim.MultiWalk.Core (OndimNode (..), Attribute)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition

getId :: [Text] -> Maybe Text
getId = asum . map (T.stripPrefix "e:")

instance OndimNode Pandoc where
  type
    ExpTypes Pandoc =
      ToSpecList
        '[ Block
         ]

instance OndimNode Block where
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
           OneSub Text
         ]
  identify (Div (_, n, _) _) = getId n
  identify (Header _ (_, n, _) _) = getId n
  identify _ = Nothing

instance OndimNode Inline where
  type
    ExpTypes Inline =
      ToSpecList
        '[ Inline,
           Block,
           Converting Attr Attribute,
           OneSub Text,
           Attribute
         ]
  identify (Span (_, n, _) _) = getId n
  identify _ = Nothing
  fromText = Just (toList . B.text)

instance Conversible Attr [Attribute] where
  convertTo (x, y, z) =
    ("id", x)
      : ("class", T.intercalate " " (filter (not . T.isPrefixOf "e:") y))
      : z
  convertFrom = foldMap go
    where
      go ("id", a) = (a, [], [])
      go ("class", a) = ("", T.split (' ' ==) a, [])
      go x = ("", [], [x])
