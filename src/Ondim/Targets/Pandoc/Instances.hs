{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Pandoc.Instances where

import Data.Aeson (encode)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Typeable (eqT, type (:~:) (..))
import Ondim
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition
import Text.Pandoc.Walk

getId :: [Text] -> Maybe Text
getId = asum . map (T.stripPrefix "e:")

instance OndimNode Pandoc where
  type ExpTypes Pandoc = 'SpecList '[ToSpec Block, ToSpec (Nesting Meta)]
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Block = Just $ \case
        (Pandoc _ b) -> b
    | Just Refl <- eqT @t @Inline = Just $ \case
        (Pandoc _ (Para i : _)) -> i
        _ -> []
    | Just Refl <- eqT @t @Rendered = Just $ one . encode
    | otherwise = Nothing

instance OndimNode Meta where
  type ExpTypes Meta = 'SpecList '[ToSpec (Trav (Map Text) (Nesting MetaValue))]

instance OndimNode MetaValue where
  type
    ExpTypes MetaValue =
      'SpecList
        '[ ToSpecSel ('ConsSel "MetaMap") (Trav (Map Text) (Nesting MetaValue)),
           ToSpecSel ('ConsSel "MetaList") MetaValue,
           ToSpecSel ('ConsSel "MetaString") (OneSub Text),
           ToSpecSel ('ConsSel "MetaInlines") Inline,
           ToSpecSel ('ConsSel "MetaBlocks") Block
         ]
  identify (MetaMap o)
    | Just (MetaString name) <- Map.lookup "$" o = Just name
  identify _ = Nothing
  children (MetaMap o)
    | Just (MetaList a) <- Map.lookup "$args" o = a
  children _ = []
  attributes (MetaMap o) = liftNodes $ Map.foldrWithKey go [] o
    where
      go k (MetaString t) a = (k, t) : a
      go _ _ a = a
  attributes _ = pure []
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . MetaString
    | otherwise = Nothing

instance OndimNode ([Inline], [[Block]]) where
  type
    ExpTypes ([Inline], [[Block]]) =
      'SpecList
        '[ ToSpec Inline,
           ToSpec (Trav [] Block)
         ]

instance OndimNode Caption where
  type
    ExpTypes Caption =
      'SpecList
        '[ ToSpec (Trav Maybe Inline),
           ToSpec Block
         ]

instance OndimNode TableHead where
  type
    ExpTypes TableHead =
      'SpecList
        '[ ToSpec (Converting Attr Attribute),
           ToSpec Row
         ]

instance OndimNode TableFoot where
  type
    ExpTypes TableFoot =
      'SpecList
        '[ ToSpec (Converting Attr Attribute),
           ToSpec Row
         ]

instance OndimNode TableBody where
  type
    ExpTypes TableBody =
      'SpecList
        '[ ToSpec (Converting Attr Attribute),
           ToSpec Row
         ]

instance OndimNode Row where
  type
    ExpTypes Row =
      'SpecList
        '[ ToSpec (Converting Attr Attribute),
           ToSpec Cell
         ]

instance OndimNode Cell where
  type
    ExpTypes Cell =
      'SpecList
        '[ ToSpec (Converting Attr Attribute),
           ToSpec Block
         ]

instance OndimNode Block where
  type
    ExpTypes Block =
      'SpecList
        '[ ToSpec Inline,
           ToSpec (Trav [] Inline),
           ToSpec Block,
           ToSpec (Trav [] Block),
           ToSpec (Trav [] (Nesting ([Inline], [[Block]]))),
           ToSpec (Nesting Caption),
           ToSpec (Nesting TableHead),
           ToSpec TableBody,
           ToSpec (Nesting TableFoot),
           ToSpec (Converting Attr Attribute),
           ToSpec (MatchWith Format (OneSub Text)),
           ToSpec (OneSub Text)
         ]
  identify (Div (_, n, _) _) = getId n
  identify (Header _ (_, n, _) _) = getId n
  identify _ = Nothing
  children = specChildren
  attributes = specAttributes
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . stringify @Block
    | Just Refl <- eqT @t @Inline = Just $ \case
        (Para i) -> i
        _ -> []
    | otherwise = Nothing
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @[Inline] = Just $ one . Plain
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

instance OndimNode Citation where
  type
    ExpTypes Citation =
      'SpecList
        '[ ToSpec (OneSub Text),
           ToSpec Inline
         ]

instance OndimNode Inline where
  type
    ExpTypes Inline =
      'SpecList
        '[ ToSpec Inline,
           ToSpec Block,
           ToSpec (Converting Attr Attribute),
           ToSpec (OneSub Text),
           ToSpec Citation,
           ToSpec (MatchWith Format (OneSub Text)),
           ToSpec (Nesting Target)
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
