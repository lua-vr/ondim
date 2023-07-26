{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Pandoc.Instances where

import Data.Aeson (encode)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Typeable (eqT, type (:~:) (..))
import Ondim
import Ondim.Extra.Substitution (SAttr, SAttrs, SText, SubstConfig (..), getSAttributes)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition
import Text.Pandoc.Walk

getId :: [Text] -> Maybe Text
getId = asum . map (T.stripPrefix "e:")

type PSConfig = 'SubstConfig '!' '(' ')'

type PText = SText PSConfig
type PAttr = SAttr PSConfig
type PAttrs = SAttrs PSConfig

data MetaMapLift

type MetaMapSub = Custom (Map Text MetaValue) MetaMapLift

instance CanLift MetaMapSub where
  liftSub (o :: Map Text MetaValue) =
    Map.fromList <$> mapMaybeM go (Map.toList o)
    where
      go (k, v)
        | Just (k', '?') <- T.unsnoc k =
            (Just . (k',) <$> liftSubstructures v)
              `catchFailure` \_ _ _ _ -> return Nothing
        | otherwise = Just . (k,) <$> liftSubstructures v

instance OndimNode Pandoc where
  type ExpTypes Pandoc = 'SpecList '[ToSpec (NL Block), ToSpec (Nesting Meta)]
  renderNode = Just encode

instance OndimNode Meta where
  type ExpTypes Meta = 'SpecList '[ToSpec MetaMapSub]

instance OndimNode MetaValue where
  type
    ExpTypes MetaValue =
      'SpecList
        '[ ToSpecSel ('ConsSel "MetaMap") MetaMapSub,
           ToSpecSel ('ConsSel "MetaList") (NL MetaValue),
           ToSpecSel ('ConsSel "MetaString") PText,
           ToSpecSel ('ConsSel "MetaInlines") (NL Inline),
           ToSpecSel ('ConsSel "MetaBlocks") (NL Block)
         ]
  identify (MetaMap o)
    | Just (MetaInlines [Str name]) <- Map.lookup "$" o = Just name
    | Just (MetaString name) <- Map.lookup "$" o = Just name
  identify _ = Nothing
  children (MetaMap o)
    | Just (MetaList a) <- Map.lookup "$args" o = a
  children _ = mempty
  attributes (MetaMap o) = liftSub @PAttrs $ Map.foldrWithKey go [] o
    where
      go k (MetaString t) a = (k, t) : a
      go _ _ a = a
  attributes _ = pure []
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . MetaString
    | Just Refl <- eqT @t @[Inline] = Just $ one . MetaInlines
    | Just Refl <- eqT @t @Pandoc = Just $ \case
        (Pandoc _ (Para i : _)) -> [MetaInlines i]
        _ -> []
    | otherwise = Nothing

instance OndimNode ([Inline], [[Block]]) where
  type
    ExpTypes ([Inline], [[Block]]) =
      'SpecList
        '[ ToSpec (NL Inline),
           ToSpec (Trav [] (NL Block))
         ]

instance OndimNode Caption where
  type
    ExpTypes Caption =
      'SpecList
        '[ ToSpec (Trav Maybe (NL Inline)),
           ToSpec (NL Block)
         ]

instance OndimNode TableHead where
  type
    ExpTypes TableHead =
      'SpecList
        '[ ToSpec (Converting Attr PAttrs),
           ToSpec (NL Row)
         ]

instance OndimNode TableFoot where
  type
    ExpTypes TableFoot =
      'SpecList
        '[ ToSpec (Converting Attr PAttrs),
           ToSpec (NL Row)
         ]

instance OndimNode TableBody where
  type
    ExpTypes TableBody =
      'SpecList
        '[ ToSpec (Converting Attr PAttrs),
           ToSpec (NL Row)
         ]

instance OndimNode Row where
  type
    ExpTypes Row =
      'SpecList
        '[ ToSpec (Converting Attr PAttrs),
           ToSpec (NL Cell)
         ]

instance OndimNode Cell where
  type
    ExpTypes Cell =
      'SpecList
        '[ ToSpec (Converting Attr PAttrs),
           ToSpec (NL Block)
         ]

instance OndimNode Block where
  type
    ExpTypes Block =
      'SpecList
        '[ ToSpec (NL Inline),
           ToSpec (Trav [] (NL Inline)),
           ToSpec (NL Block),
           ToSpec (Trav [] (NL Block)),
           ToSpec (Trav [] (Nesting ([Inline], [[Block]]))),
           ToSpec (Nesting Caption),
           ToSpec (Nesting TableHead),
           ToSpec (NL TableBody),
           ToSpec (Nesting TableFoot),
           ToSpec (Converting Attr PAttrs),
           ToSpec (MatchWith Format PText),
           ToSpec PText
         ]
  identify (Div (_, n, _) _) = getId n
  identify (Header _ (_, n, _) _) = getId n
  identify _ = Nothing
  children = getSubstructure
  attributes = getSAttributes @PSConfig
  nodeAsText = Just $ stringify @Block
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @[Inline] = Just $ one . Plain
    | Just Refl <- eqT @t @Pandoc = Just $ \case
        (Pandoc _ b) -> b
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
        '[ ToSpec PText,
           ToSpec (NL Inline)
         ]

instance OndimNode Inline where
  type
    ExpTypes Inline =
      'SpecList
        '[ ToSpec (NL Inline),
           ToSpec (NL Block),
           ToSpec (Converting Attr PAttrs),
           ToSpec PText,
           ToSpec (NL Citation),
           ToSpec (MatchWith Format PText),
           ToSpec PAttr
         ]
  identify (Span (_, n, _) _) = getId n
  identify _ = Nothing
  children = getSubstructure
  attributes = getSAttributes @PSConfig
  nodeAsText = Just $ stringify @Inline
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ toList . B.text
    | Just Refl <- eqT @t @Pandoc = Just $ \case
        (Pandoc _ (Para i : _)) -> i
        _ -> []
    | Just Refl <- eqT @t @Block = Just $ \case
        (Para i) -> i
        _ -> []
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
