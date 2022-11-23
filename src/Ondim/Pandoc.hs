{-# LANGUAGE UndecidableInstances #-}

module Ondim.Pandoc where

import Data.Text qualified as T
import Ondim
import Ondim.Extra.Expansions
  ( Attribute,
    ExpansibleText,
    ignore, switchBound, ifBound, bind, scope, bindText, attrSub, HasAttrs,
  )
import Ondim.MultiWalk.Combinators
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Map.Syntax ((##))

data PandocTag

getId :: [Text] -> Maybe Text
getId = asum . map (T.stripPrefix "e:")

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

cons :: forall m. Monad m => Expansion PandocTag m Block
cons x = do
  nodes <- liftChildren x
  pure $
    fromMaybe nodes do
      (h0 :| nodes') <- nonEmpty nodes
      (h1 :| nodes'') <- nonEmpty nodes'
      let f :: [Inline] -> [Inline]
          f y = getSubstructure @Inline h0 ++ y
      pure $ modSubstructure @Inline f h1 : nodes''

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim PandocTag m t ->
  Ondim PandocTag m t
bindDefaults st =
  st
    `binding` do
      "if" ## ifBound @Block
      "switch" ## switchBound
      "bind" ## bind
      "scope" ## scope
      "bind-text" ## bindText stringify
      "cons" ## cons
    `binding` do
      "if" ## ifBound @Inline
      "switch" ## switchBound
      "bind" ## bind
      "scope" ## scope
      "bind-text" ## bindText stringify
    `bindingFilters` do
      "attrSub" ## attrSub

-- Template loading helpers

blockFromDocument :: Monad m => Text -> Pandoc -> Expansion PandocTag m Block
blockFromDocument name (Pandoc _ b) = fromTemplate name b

inlineFromDocument :: Monad m => Text -> Pandoc -> Expansion PandocTag m Inline
inlineFromDocument name (Pandoc _ (Para i : _)) = fromTemplate name i
inlineFromDocument _ _ = ignore

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
