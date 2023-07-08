{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Pandoc.Instances where

import Data.Text qualified as T
import Ondim
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Typeable (eqT, type (:~:) (..))

getId :: [Text] -> Maybe Text
getId = asum . map (T.stripPrefix "e:")

instance OndimNode Pandoc where
  type
    ExpTypes Pandoc =
      '[ ToSpec Block
       ]

instance OndimNode ([Inline], [[Block]]) where
  type ExpTypes ([Inline], [[Block]]) =
    '[ ToSpec Inline,
       ToSpec (Trav [] Block)
     ]

instance OndimNode Block where
  type
    ExpTypes Block =
      '[ ToSpec Inline,
         ToSpec (Trav [] Inline),
         ToSpec Block,
         ToSpec (Trav [] Block),
         ToSpec (Nesting ([Inline], [[Block]])),
         ToSpec (Converting Attr Attribute),
         ToSpec (OneSub Text)
       ]
  identify (Div (_, n, _) _) =  getId n
  identify (Header _ (_, n, _) _) = getId n
  identify _ = Nothing
  children = specChildren
  attributes = specAttributes
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . stringify @Block
    | otherwise = Nothing

instance Conversible Attr [Attribute] where
  convertTo (x, y, z) =
    ("id", x)
      : ("class", T.intercalate " " (filter (not . T.isPrefixOf "e:") y))
      : z
  updateFrom _ = foldMap go
    where
      go ("id", a) = (a, [], [])
      go ("class", a) = ("", T.split (' ' ==) a, [])
      go x = ("", [], [x])

instance OndimNode Inline where
  type
    ExpTypes Inline =
      '[ ToSpec Inline,
         ToSpec Block,
         ToSpec (Converting Attr Attribute),
         ToSpec (OneSub Text)
       ]
  identify (Span (_, n, _) _) = getId n
  identify _ = Nothing
  children = specChildren
  attributes = specAttributes
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . stringify @Inline
    | otherwise = Nothing
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ toList . B.text
    | otherwise = Nothing

-- Miscellaneous (from Text.Pandoc.Shared)

stringify :: Walkable Inline a => a -> T.Text
stringify = query go
  where
    go :: Inline -> T.Text
    go Space = " "
    go SoftBreak = " "
    go (Str x) = x
    go (Code _ x) = x
    go (Math _ x) = x
    go LineBreak = " "
    go _ = ""
