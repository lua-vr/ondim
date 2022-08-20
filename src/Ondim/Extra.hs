{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Uselful examples of expansions.
module Ondim.Extra where

import Data.Attoparsec.Text (Parser, char, string, takeTill)
import Data.List qualified as L
import Data.Map.Syntax
import Ondim
import Replace.Attoparsec.Text (streamEditT)

-- Aliases for attributes

type Attribute = (Text, Text)

type ExpansibleText = Text

attributes ::
  forall t tag.
  (HasSub tag t Attribute, OndimNode tag Attribute) =>
  Ondim tag t ->
  Ondim tag [Attribute]
attributes = liftNodes <=< inhibitingExpansions . (getSubs @tag <$>)

type HasAttrChild tag t =
  ( OndimNode tag t,
    OndimNode tag Attribute,
    HasSub tag t t,
    HasSub tag t Attribute
  )

-- Expansions

ignore :: forall t tag. OndimTag tag => Expansion tag t
ignore = const $ pure []

ifElse ::
  forall t tag.
  (OndimNode tag t, HasSub tag t t) =>
  Bool ->
  Expansion tag t
ifElse cond node = do
  els <- inhibitingExpansions $ children node
  let (yes, drop 1 -> no) =
        break ((Just "else" ==) . identify @tag) els
  if cond
    then liftNodes yes
    else liftNodes no
{-# INLINEABLE ifElse #-}

getTag :: [Attribute] -> Maybe Text
getTag attrs = L.lookup "tag" attrs <|> (case attrs of [(s, "")] -> Just s; _ -> Nothing)

switchCases :: forall t tag. HasAttrChild tag t => Text -> Expansions' tag t
switchCases tag =
  "case" ## \caseNode -> do
    attrs <- attributes caseNode
    withoutExpansion @t "case" $
      if Just tag == getTag attrs
        then children caseNode
        else pure []
{-# INLINEABLE switchCases #-}

switch ::
  forall tag t.
  (HasAttrChild tag t) =>
  Text ->
  Expansion tag t
switch tag node =
  children node
    `binding` switchCases @t tag

switchWithDefault ::
  forall tag t.
  (HasAttrChild tag t) =>
  Text ->
  Expansion tag t
switchWithDefault tag node = do
  els <- inhibitingExpansions $ children node
  fromMaybe (pure []) do
    child <-
      find (\x -> nameIs "case" x && hasTag x) els
        <|> find (\x -> nameIs "default" x) els
    pure $ liftNodes (getSubs @tag child)
  where
    nameIs n x = identify @tag x == Just n
    hasTag (getSubs @tag -> attrs) =
      Just tag == getTag attrs

ifBound :: forall t tag. HasAttrChild tag t => Expansion tag t
ifBound node = do
  attrs <- attributes node
  bound <- case getTag attrs of
    Just tag -> isJust <$> getExpansion @t tag
    Nothing -> pure False
  ifElse bound node

switchBound :: forall t tag. HasAttrChild tag t => Expansion tag t
switchBound node = do
  tag <- getTag <$> attributes node
  fromMaybe (children node) do
    tagC <- callText @tag <$> tag
    pure $ (`switchWithDefault` node) =<< tagC

-- Binding

-- | This expansion works like Heist's `bind` splice
bind :: forall t tag. HasAttrChild tag t => Expansion tag t
bind node = do
  attrs <- attributes node
  whenJust (getTag attrs) $ \tag -> do
    putExpansion tag $ \inner ->
      children node
        `binding` do
          "apply-content" ## const (children inner)
  pure []

-- | This expansion works like Heist's `bind` splice, but binds what's inside as
--  text (via the toTxt parameter).
bindText ::
  (OndimNode tag Attribute, HasSub tag t Attribute) =>
  (t -> Text) ->
  Expansion tag t
bindText toTxt node = do
  attrs <- attributes node
  whenJust (getTag attrs) $ \tag -> do
    putTextExp tag $ toTxt <$> node
  pure []

-- | This expansion does nothing, and simply return its children. This means that
--   its only purpose is to create a new scope for its children.
--
--   It can be used to call other expansions with "arguments":
--
--   > <bind animal-entry>There is a <animal /> with age <age /></bind>
--   >
--   > <scope>
--   >   <bind animal>Lion</bind>
--   >   <bind age>9 years</bind>
--   >   <animal-entry />
--   > <scope/>
scope :: forall t tag. HasAttrChild tag t => Expansion tag t
scope = children

-- | Substitution of !(name) in attribute text
interpParser :: Parser Text
interpParser = do
  _ <- string "!("
  s <- takeTill (== ')')
  _ <- char ')'
  pure s

attrEdit :: OndimTag tag => Text -> Ondim tag Text
attrEdit = streamEditT interpParser callText

attrSub :: OndimTag tag => Filter tag ExpansibleText
attrSub = (mapM attrEdit =<<)
