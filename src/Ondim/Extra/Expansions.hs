{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

-- | Uselful examples of expansions.
module Ondim.Extra.Expansions where

import Control.Monad.RWS (censor)
import Data.Attoparsec.Text (Parser, char, string, takeTill)
import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import Ondim
import Ondim.MultiWalk.Core (GlobalConstraints, SomeExpansion (TextData), toSomeExpansion)
import Replace.Attoparsec.Text (streamEditT)

attributes ::
  forall t tag m.
  (OndimNode tag t, Monad m, OndimTag tag) =>
  t ->
  Ondim tag m [Attribute]
attributes = liftNodes . getAttrs @tag

lookupAttr ::
  (Monad m, OndimNode tag t, OndimTag tag) =>
  Text ->
  t ->
  Ondim tag m (Maybe Text)
lookupAttr key = fmap (L.lookup key) . attributes

lookupAttr' ::
  (Monad m, OndimNode tag t, OndimTag tag) =>
  Text ->
  t ->
  Ondim tag m Text
lookupAttr' key node =
  maybe (throwCustom $ "Missing '" <> key <> "' argument.") pure . L.lookup key
    =<< attributes node

-- Adding prefixes

{- | Simple convenience function for prefixing expansion names, useful for
   simulating namespaces.
-}
prefixed :: Text -> ExpansionMap tag m -> ExpansionMap tag m
prefixed pfx = censor (Map.mapKeys (pfx <>))

-- Expansions

ignore :: forall t tag m. (OndimTag tag, Monad m) => Expansion tag m t
ignore = const $ pure []

with :: forall t tag m. GlobalConstraints tag m t => Expansion tag m t
with node = do
  tag <- lookupAttr' "exp" node
  oldExp :: Expansion tag m t <-
    maybe (throwNotBound tag) pure
      =<< getExpansion tag
  as <- lookupAttr' "as" node
  noOverwrite <- isJust <$> lookupAttr "no-overwrite" node
  exists <- isJust <$> getExpansion @t as
  if noOverwrite && exists
    then liftChildren node
    else liftChildren node `binding` (as ## oldExp)

ifElse ::
  forall t tag m.
  ( OndimNode tag t,
    OndimTag tag,
    Monad m
  ) =>
  Bool ->
  Expansion tag m t
ifElse cond node = do
  let els = children @tag node
      yes = filter ((Just "o:else" /=) . identify @tag) els
      no =
        maybe [] (children @tag) $
          find ((Just "o:else" ==) . identify @tag) els
  if cond
    then liftNodes yes
    else liftNodes no
{-# INLINEABLE ifElse #-}

getTag :: [Attribute] -> Maybe Text
getTag attrs = L.lookup "tag" attrs <|> (case attrs of [(s, "")] -> Just s; _ -> Nothing)

switchCases :: forall t tag m. GlobalConstraints tag m t => Text -> ExpansionMap tag m
switchCases tag =
  "o:case" ## \caseNode -> do
    attrs <- attributes @t caseNode
    withoutExpansions ["o:case"] $
      if Just tag == getTag attrs
        then liftChildren caseNode
        else pure []
{-# INLINEABLE switchCases #-}

switch ::
  forall tag m t.
  GlobalConstraints tag m t =>
  Text ->
  Expansion tag m t
switch tag node =
  liftChildren node
    `binding` switchCases @t tag

switchWithDefault ::
  forall tag m t.
  GlobalConstraints tag m t =>
  Text ->
  Expansion tag m t
switchWithDefault tag node = do
  let els = children @tag node
  fromMaybe (pure []) do
    child <-
      find (\x -> nameIs "o:case" x && hasTag x) els
        <|> find (nameIs "o:default") els
    pure $ liftChildren child
  where
    nameIs n x = identify @tag x == Just n
    hasTag (getAttrs @tag -> attrs) =
      Just tag == getTag attrs

ifBound :: forall t tag m. GlobalConstraints tag m t => Expansion tag m t
ifBound node = do
  attrs <- attributes node
  bound <- case getTag attrs of
    Just tag -> isJust <$> getExpansion @t tag
    Nothing -> pure False
  ifElse bound node

switchBound :: forall t tag m. GlobalConstraints tag m t => Expansion tag m t
switchBound node = do
  tag <- getTag <$> attributes node
  flip (maybe $ pure []) tag \tag' -> do
    tagC <- fromMaybe "o:default" <$> getTextData tag'
    switchWithDefault tagC node

-- Binding

-- | This expansion works like Heist's `bind` splice
bind :: forall t tag m. GlobalConstraints tag m t => Expansion tag m t
bind node = do
  attrs <- attributes node
  whenJust (getTag attrs) $ \name -> do
    putExpansion name $ toSomeExpansion $ \inner -> do
      attrs' <- attributes inner
      liftChildren node
        `binding` do
          (name <> ":content") ## const (liftChildren inner)
          forM_ attrs' \attr ->
            name <> ":" <> fst attr #@ snd attr
  pure []

{- | This expansion works like Heist's `bind` splice, but binds what's inside as
  text (via the toTxt parameter).
-}
bindText ::
  forall tag m t.
  GlobalConstraints tag m t =>
  (t -> Text) ->
  Expansion tag m t
bindText toTxt self = do
  attrs <- attributes self
  whenJust (getTag attrs) $ \tag -> do
    putExpansion tag $ TextData $ foldMap toTxt $ children @tag self
  pure []

{- | This expansion creates a new scope for the its children, in the sense that
 the inner state does not leak outside.

  For this reason, it can be used to call other expansions with "arguments":

   > <bind animal-entry>There is a <animal /> with age <age /></bind>
   >
   > <scope>
   >   <bind animal>Lion</bind>
   >   <bind age>9 years</bind>
   >   <animal-entry />
   > <scope/>
-}
scope :: forall t tag m. GlobalConstraints tag m t => Expansion tag m t
scope node = do
  s <- getOndimS
  liftChildren node <* putOndimS s

-- | Substitution of !(name) in attribute text
interpParser :: Parser Text
interpParser = do
  _ <- string "!("
  s <- takeTill (== ')')
  _ <- char ')'
  pure s

attrEdit :: (OndimTag tag, Monad m) => Text -> Ondim tag m Text
attrEdit = streamEditT interpParser callText

attrSub :: (OndimTag tag, Monad m) => Filter tag m Text
attrSub = (mapM attrEdit =<<)
