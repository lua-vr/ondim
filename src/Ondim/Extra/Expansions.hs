{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# HLINT ignore "Use uncurry" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Uselful examples of expansions.
module Ondim.Extra.Expansions where

import Control.Monad.Except (MonadError (throwError), catchError, throwError)
import Data.Attoparsec.Text (Parser, char, string, takeTill)
import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import Data.Text qualified as T
import Ondim
import Relude.Extra.Map (notMember)
import Replace.Attoparsec.Text (streamEditT)

lookupAttr' ::
  (Monad m, OndimNode tag t) =>
  Text ->
  t ->
  Ondim tag m Text
lookupAttr' key node =
  maybe (throwCustom $ "Missing '" <> key <> "' argument.") pure . L.lookup key
    =<< attributes node

-- * Expansions

ignore :: forall t tag m. Monad m => Expansion tag m t
ignore = const $ pure []

debug :: GlobalExpansion tag m
debug node = do
  exps <- expansions <$> getOndimS
  let go (Expansions e) = (`foldMap` Map.toList e) \(k, v) ->
        case v of
          Namespace m -> (k, "Namespace") : map (first ((k <> ".") <>)) (go m)
          SomeExpansion {} -> one (k, "SomeExpansion")
          GlobalExpansion {} -> one (k, "GlobalExpansion")
          TextData {} -> one (k, "TextData")
      keys = go exps
  join <$> forM keys \(key, kind) ->
    liftChildren node `binding` do
      "key" #@ key
      "kind" #@ kind

open :: GlobalExpansion tag m
open node = do
  name' <- viaNonEmpty (fst . head) <$> attributes node
  name <- maybe (throwCustom "Expansion name not provided.") pure name'
  exps <- lookupExpansion name . expansions <$> getOndimS
  withExpansion name Nothing $
    case exps of
      Just (Namespace v) -> withExpansions v $ liftChildren node
      _ -> throwNotBound name

with :: GlobalExpansion tag m
with node = do
  name' <- viaNonEmpty (fst . head) <$> attributes node
  name <- maybe (throwCustom "Expansion name not provided.") pure name'
  exps <- expansions <$> getOndimS
  as <- fromMaybe "this" <$> lookupAttr "as" node
  noOverwrite <- isJust <$> lookupAttr "no-overwrite" node
  let expansion = lookupExpansion name exps
      exists = isJust $ lookupExpansion as exps
  withExpansion name Nothing $
    if noOverwrite && exists
      then liftChildren node
      else withExpansion as expansion $ liftChildren node

ifElse ::
  forall t tag m.
  ( OndimNode tag t,
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

switchCases :: forall tag m. Text -> ExpansionMap tag m
switchCases tag =
  "o:case" #* \(caseNode :: t) -> do
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
    `binding` switchCases tag
{-# INLINEABLE switch #-}

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
          "this.children" ## const (liftChildren inner)
          forM_ attrs' \attr ->
            "this." <> fst attr #@ snd attr
  pure []

{- | This expansion works like Heist's `bind` splice, but binds what's inside as
  text (via the toTxt parameter).
-}
bindText ::
  GlobalConstraints tag m t =>
  (t -> Text) ->
  Expansion tag m t
bindText toTxt self = do
  attrs <- attributes self
  child <- liftChildren self
  whenJust (getTag attrs) $ \tag -> do
    putExpansion tag $ TextData $ foldMap toTxt child
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

-- * Filters

-- | Substitution of !(name) in attribute text
interpParser :: Parser Text
interpParser = do
  _ <- string "!("
  s <- takeTill (== ')')
  _ <- char ')'
  pure s

attrEdit :: Monad m => Text -> Ondim tag m Text
attrEdit = streamEditT interpParser callText

attrSub :: Monad m => Filter tag m Text
attrSub t _ = one <$> attrEdit t

notBoundFilter :: forall t tag m. (GlobalConstraints tag m t) => Set Text -> Filter tag m t
notBoundFilter validIds original nodes
  | any (("@try" ==) . fst) (getAttrs @tag original) =
      result `catchError` \case
        ExpansionNotBound {} -> return []
        e -> throwError e
  | otherwise = result
  where
    result =
      nodes >>= mapM \node ->
        case identify @tag node of
          Just name | name `notMember` validIds -> throwNotBound name
          _ -> return node

mbAttrFilter :: Monad m => Filter tag m Attribute
mbAttrFilter (k, _) x
  | Just k' <- "?" `T.stripSuffix` k =
      first (const k') <<$>> x `catchError` \case
        ExpansionNotBound {} -> return []
        e -> throwError e
  | otherwise = x
