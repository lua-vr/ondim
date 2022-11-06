{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Uselful examples of expansions.
module Ondim.Extra.Expansions where

import Data.Attoparsec.Text (Parser, char, string, takeTill)
import Data.List qualified as L
import Data.Map.Syntax
import Ondim
import Replace.Attoparsec.Text (streamEditT)

-- Alias for attributes

type Attribute = (Text, Text)

type ExpansibleText = Text

class HasAttrs tag t where
  getAttrs :: t -> [Attribute]
  default getAttrs ::
    ( OndimNode tag t,
      Ondim.All (Substructure Attribute) (ExpTypes t)
    ) =>
    t ->
    [Attribute]
  getAttrs = getSubstructure @Attribute

attributes ::
  forall t tag m.
  (HasAttrs tag t, OndimNode tag Attribute, Monad m, OndimTag tag) =>
  Ondim tag m t ->
  Ondim tag m [Attribute]
attributes = liftNodes <=< inhibitingExpansions . (getAttrs @tag <$>)

type HasAttrChild tag t =
  ( OndimNode tag t,
    OndimNode tag Attribute,
    OndimTag tag,
    HasAttrs tag t
  )

-- Adding prefixes

-- | Simple convenience function for prefixing expansion names, useful for
--   simulating namespaces.
prefixed :: Text -> MapSyntax Text a -> MapSyntax Text a
prefixed pfx = mapK (pfx <>)

-- Expansions

ignore :: forall t tag m. (OndimTag tag, Monad m) => Expansion tag m t
ignore = const $ pure []

ifElse ::
  forall t tag m.
  ( OndimNode tag t,
    OndimTag tag,
    Monad m
  ) =>
  Bool ->
  Expansion tag m t
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

switchCases :: forall t tag m. (HasAttrChild tag t, Monad m) => Text -> Expansions' tag m t
switchCases tag =
  "case" ## \caseNode -> do
    attrs <- attributes caseNode
    withoutExpansion @t "case" $
      if Just tag == getTag attrs
        then children caseNode
        else pure []
{-# INLINEABLE switchCases #-}

switch ::
  forall tag m t.
  (HasAttrChild tag t, Monad m) =>
  Text ->
  Expansion tag m t
switch tag node =
  children node
    `binding` switchCases @t tag

switchWithDefault ::
  forall tag m t.
  (HasAttrChild tag t, Monad m) =>
  Text ->
  Expansion tag m t
switchWithDefault tag node = do
  els <- inhibitingExpansions $ children node
  fromMaybe (pure []) do
    child <-
      find (\x -> nameIs "case" x && hasTag x) els
        <|> find (nameIs "default") els
    pure $ liftNodes (children' @tag child)
  where
    nameIs n x = identify @tag x == Just n
    hasTag (getAttrs @tag -> attrs) =
      Just tag == getTag attrs

ifBound :: forall t tag m. (HasAttrChild tag t, Monad m) => Expansion tag m t
ifBound node = do
  attrs <- attributes node
  bound <- case getTag attrs of
    Just tag -> isJust <$> getExpansion @t tag
    Nothing -> pure False
  ifElse bound node

switchBound :: forall t tag m. (HasAttrChild tag t, Monad m) => Expansion tag m t
switchBound node = do
  tag <- getTag <$> attributes node
  flip (maybe $ pure []) tag \tag' -> do
    exp' <- getTextExpansion tag'
    tagC <- fromMaybe (pure "default") exp'
    switchWithDefault tagC node

-- Binding

-- | This expansion works like Heist's `bind` splice
bind :: forall t tag m. (HasAttrChild tag t, Monad m) => Expansion tag m t
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
  (HasAttrChild tag t, Monad m) =>
  (t -> Text) ->
  Expansion tag m t
bindText toTxt node = do
  attrs <- attributes node
  whenJust (getTag attrs) $ \tag -> do
    putTextExpansion tag $ toTxt <$> node
  pure []

-- | This expansion creates a new scope for the its children, in the sense that
-- the inner state does not leak outside.
--
--  For this reason, it can be used to call other expansions with "arguments":
--
--   > <bind animal-entry>There is a <animal /> with age <age /></bind>
--   >
--   > <scope>
--   >   <bind animal>Lion</bind>
--   >   <bind age>9 years</bind>
--   >   <animal-entry />
--   > <scope/>
scope :: forall t tag m. (HasAttrChild tag t, Monad m) => Expansion tag m t
scope = withOndimGS id . children

-- | Substitution of !(name) in attribute text
interpParser :: Parser Text
interpParser = do
  _ <- string "!("
  s <- takeTill (== ')')
  _ <- char ')'
  pure s

attrEdit :: (OndimTag tag, Monad m) => Text -> Ondim tag m Text
attrEdit = streamEditT interpParser callText

attrSub :: (OndimTag tag, Monad m) => Filter tag m ExpansibleText
attrSub = (mapM attrEdit =<<)
