{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Examples of expansions.

module Ondim.Extra where
import Ondim
import Data.Map.Syntax
import Data.List qualified as L
import Data.Attoparsec.Text (Parser, string, takeTill, char)
import Replace.Attoparsec.Text (streamEditT)

-- Aliases for attributes

type Attribute = (Text, Text)

type ExpansibleText = Text

attributes :: forall t tag. (OndimTag tag, HasSub tag t Attribute) =>
  Ondim tag t -> Ondim tag [Attribute]
attributes = inhibitingExpansions . fmap (getSubs @tag)

type HasAttrChild tag t = (OndimNode tag t, HasSub tag t t, HasSub tag t Attribute)

-- Expansions

ignore :: forall t tag. OndimTag tag => Expansion tag t
ignore = const $ pure []

ifElse :: forall tag t.
  (OndimNode tag t, HasSub tag t t) =>
  Bool -> Expansion tag t
ifElse cond node = do
  els <- inhibitingExpansions $ children node
  let (yes, drop 1 -> no) =
        break ((Just "else" ==) . identify @tag) els
  if cond
    then foldMapM liftNode yes
    else foldMapM liftNode no

switchCases :: forall t tag. HasAttrChild tag t => Text -> Expansions' tag t
switchCases tag = do
  "case" ## \caseNode -> do
    attrs <- attributes caseNode
    if isJust (L.lookup tag attrs) || Just tag == L.lookup "tag" attrs
    then children caseNode
    else pure []
{-# INLINABLE switchCases #-}

switch :: forall tag t. (HasAttrChild tag t) =>
  Text -> Expansion tag t
switch tag node = children node
  `binding` switchCases @t tag

switchWithDefault :: forall tag t. (HasAttrChild tag t) =>
  Text -> Expansion tag t
switchWithDefault tag node = do
  els <- inhibitingExpansions $ children node
  fromMaybe (pure []) do
    child <- find (\x -> nameIs "case" x && hasTag x) els <|>
             find (\x -> nameIs "default" x) els
    pure $ foldMapM liftNode (getSubs @tag child)
  where
    nameIs n x = identify @tag x == Just n
    hasTag (getSubs @tag -> attrs) =
      isJust (L.lookup tag attrs) ||
      Just tag == L.lookup "tag" attrs


-- Implementations

ifBound :: forall t tag.
  HasAttrChild tag t => Expansion tag t
ifBound node = do
  attrs <- attributes node
  bound <- case L.lookup "tag" attrs of
    Just tag -> isJust <$> getExpansion @t tag
    Nothing -> pure False
  ifElse bound node

switchBound :: forall t tag. HasAttrChild tag t => Expansion tag t
switchBound node = do
  tag <- L.lookup "tag" <$> attributes node
  fromMaybe (children node) do
    tagC <- callText @tag <$> tag
    pure $ (`switchWithDefault` node) =<< tagC

-- | This expansion works like Heist's `bind` splice
bind :: forall t tag.
  ( OndimNode tag t
  , HasSub tag t t
  , HasSub tag t Attribute
  ) => Expansion tag t
bind node = do
  attrs <- attributes node
  whenJust (L.lookup "tag" attrs) $ \tag -> do
    putExpansion tag $ \inner ->
      children node
      `binding` do
        "apply-content" ## const (children inner)
  pure []

-- Substitution of ${name} in attribute text

interpParser :: Parser Text
interpParser = do
  _ <- string "${"
  s <- takeTill (== '}')
  _ <- char '}'
  pure s

attrEdit :: OndimTag tag => Text -> Ondim tag Text
attrEdit = streamEditT interpParser callText

expandAttr :: OndimTag tag => Expansion tag ExpansibleText
expandAttr = fmap one . (attrEdit =<<)
