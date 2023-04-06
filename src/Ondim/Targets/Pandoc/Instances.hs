{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Pandoc.Instances where

import Data.Text qualified as T
import Ondim (Attribute, OndimNode (..), OneSub, ToSpec, Under, Converting, Conversible (..), SelSpec (..))
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition

getId :: [Text] -> Maybe Text
getId = asum . map (T.stripPrefix "e:")

instance OndimNode Pandoc where
  type
    ExpTypes Pandoc =
      '[ ToSpec Block
       ]

instance OndimNode Block where
  type
    ExpTypes Block =
      '[ ToSpec Inline,
         ToSpec (Under [Inline] 'NoSel Inline),
         ToSpec Block,
         ToSpec (Under [Block] 'NoSel Block),
         ToSpec (Under ([Inline], [[Block]]) 'NoSel (Under [Inline] 'NoSel Inline)),
         ToSpec (Under ([Inline], [[Block]]) 'NoSel (Under [[Block]] 'NoSel (Under [Block] 'NoSel Block))),
         ToSpec (Under Attr 'NoSel Attribute),
         ToSpec (OneSub Text)
       ]
  identify (Div (_, n, _) _) = getId n
  identify (Header _ (_, n, _) _) = getId n
  identify _ = Nothing

instance OndimNode Inline where
  type
    ExpTypes Inline =
        '[ ToSpec Inline,
           ToSpec Block,
           ToSpec (Converting Attr Attribute),
           ToSpec (OneSub Text),
           ToSpec Attribute
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
