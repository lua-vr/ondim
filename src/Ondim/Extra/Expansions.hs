{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Uselful examples of expansions.
module Ondim.Extra.Expansions where

import Control.Monad.Except (MonadError (throwError), catchError, throwError)
import Control.Monad.Writer.CPS (censor)
import Data.Attoparsec.Text (Parser, char, string, takeTill)
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Ondim
import Ondim.MultiWalk.Basic (Expansions (..), SomeExpansion (..))
import Replace.Attoparsec.Text (streamEditT)

lookupAttr' ::
  (Monad m, OndimNode t) =>
  Text ->
  t ->
  Ondim m Text
lookupAttr' key node =
  maybe (throwCustom $ "Missing '" <> key <> "' argument.") pure . L.lookup key
    =<< attributes node

getSingleAttr :: Text -> [Attribute] -> Maybe Text
getSingleAttr name attrs = L.lookup name attrs <|> viaNonEmpty (fst . head) attrs

identifiesAs :: OndimNode t => [Text] -> t -> Bool
identifiesAs n = (Just n ==) . fmap splitExpansionKey . identify

-- * Expansions

ignore :: forall t m. Monad m => Expansion m t
ignore = const $ pure []

debug :: GlobalExpansion m
debug node = do
  exps <- expansions <$> getOndimS
  let go (Expansions e) = (`foldMap` HMap.toList e) \(k, v) ->
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

open :: GlobalExpansion m
open node = do
  name' <- viaNonEmpty (fst . head) <$> attributes node
  name <- maybe (throwCustom "Expansion name not provided.") pure name'
  exps <- lookupExpansion name . expansions <$> getOndimS
  withExpansion name Nothing $
    case exps of
      Just (Namespace v) -> withExpansions v $ liftChildren node
      _ -> throwNotBound name

with :: GlobalExpansion m
with node = do
  exps <- expansions <$> getOndimS
  actions <-
    attributes node <&> map \(k, v) ->
      let expansion = lookupExpansion v exps
       in withExpansion v Nothing . withExpansion k expansion
  foldr ($) (liftChildren node) actions

listExp ::
  (a -> SomeExpansion m) ->
  [a] ->
  ExpansionMap m
listExp f list = do
  "size" #@ show $ length list
  "list" #* listList f list
  "nth" #* nthList f list
  whenJust (viaNonEmpty head list) (("head" #:) . f)

listList ::
  forall a m t.
  GlobalConstraints m t =>
  (a -> SomeExpansion m) ->
  [a] ->
  Expansion m t
listList f list node = do
  alias <- fromMaybe "item" <$> lookupAttr "as" node
  intercalateWith <- lookupAttr "intercalate" node
  let inter txt
        | Just ft <- fromText @t = intercalate (ft txt)
        | otherwise = join
      join' = maybe join inter intercalateWith
  withExpansion alias Nothing $
    join' <$> forM list \el ->
      liftChildren node
        `binding` do alias #: f el

nthList ::
  forall a m t.
  GlobalConstraints m t =>
  (a -> SomeExpansion m) ->
  [a] ->
  Expansion m t
nthList f list node = do
  n <- getSingleAttr "n" <$> attributes node
  el <- fromMaybe (throwCustom $ err n) do
    n' <- readMaybe . toString =<< n
    pure <$> maybeAt n' list
  case getSomeExpansion $ f el of
    Just e -> e node
    Nothing -> throwCustom (err n)
  where
    err n = "List index error: no such index " <> show n

assocsExp ::
  (v -> SomeExpansion m) ->
  [(Text, v)] ->
  ExpansionMap m
assocsExp vf obj = do
  "size" #@ show $ length obj
  "list" #* listList kv obj
  "keys" #* listList textData (map fst obj)
  "values" #* listList vf (map snd obj)
  forM_ obj (\(k, v) -> k #: vf v)
  where
    kv (k, v) =
      namespace do
        "key" #@ k
        "value" #: vf v

mapExp ::
  (v -> SomeExpansion m) ->
  Map Text v ->
  ExpansionMap m
mapExp vf obj = assocsExp vf (Map.toList obj)

ifElse ::
  forall t m.
  ( OndimNode t,
    Monad m
  ) =>
  Bool ->
  Expansion m t
ifElse cond node = do
  let els = children node
      yes = filter (not . identifiesAs ["o", "else"]) els
      no =
        maybe [] children $
          find (identifiesAs ["o", "else"]) els
  if cond
    then liftNodes yes
    else liftNodes no
{-# INLINEABLE ifElse #-}

switchCases :: forall m. Text -> ExpansionMap m
switchCases tag =
  "o:case" #* \(caseNode :: t) -> do
    attrs <- attributes @t caseNode
    withoutExpansions ["o:case"] $
      if Just tag == getSingleAttr "tag" attrs
        then liftChildren caseNode
        else pure []
{-# INLINEABLE switchCases #-}

switch ::
  forall m t.
  GlobalConstraints m t =>
  Text ->
  Expansion m t
switch tag node =
  liftChildren node
    `binding` switchCases tag
{-# INLINEABLE switch #-}

switchWithDefault ::
  forall m t.
  GlobalConstraints m t =>
  Text ->
  Expansion m t
switchWithDefault tag node = do
  let els = children node
  match <- (`findM` els) \x -> do
    caseTag <- getSingleAttr "tag" <$> attributes x
    return $ identifiesAs ["o", "case"] x && caseTag == Just tag
  fromMaybe (pure []) do
    child <- match <|> find (identifiesAs ["o", "default"]) els
    pure $ liftChildren child
  where
    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

ifBound :: forall t m. GlobalConstraints m t => Expansion m t
ifBound node = do
  attrs <- attributes node
  bound <- case getSingleAttr "exp" attrs of
    Just tag -> isJust <$> getExpansion @t tag
    Nothing -> pure False
  ifElse bound node

switchBound :: forall t m. GlobalConstraints m t => Expansion m t
switchBound node = do
  tag <- getSingleAttr "exp" <$> attributes node
  flip (maybe $ pure []) tag \tag' -> do
    tagC <- fromMaybe "" <$> getTextData tag'
    switchWithDefault tagC node

-- Binding

-- | This expansion works like Heist's `bind` splice
bind :: forall t m. GlobalConstraints m t => Expansion m t
bind node = do
  attrs <- attributes node
  whenJust (getSingleAttr "name" attrs) $ \name -> do
    putExpansion name $ someExpansion $ \inner -> do
      attrs' <- attributes inner
      liftChildren node
        `binding` do
          -- Note to self: writing "this.children" is intentional. rebember that
          -- using `#.` would mean other stuff under the namespace is erased. We
          -- don't want that.
          "this.children" ## const (liftChildren inner)
          censor (map $ first ("this." <>)) $ assocsExp textData attrs'
  pure []

{- | This expansion works like Heist's `bind` splice, but binds what's inside as
  text (via the toTxt parameter).
-}
bindText ::
  GlobalConstraints m t =>
  (t -> Text) ->
  Expansion m t
bindText toTxt self = do
  attrs <- attributes self
  child <- liftChildren self
  whenJust (getSingleAttr "name" attrs) $ \tag -> do
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
scope :: forall t m. GlobalConstraints m t => Expansion m t
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

attrEdit :: Monad m => Text -> Ondim m Text
attrEdit = streamEditT interpParser callText

attrSub :: Monad m => Filter m Text
attrSub t _ = one <$> attrEdit t

notBoundFilter :: forall t m. (GlobalConstraints m t) => Set Text -> Filter m t
notBoundFilter validIds original nodes = do
  attrs <- attributes original
  if any (("@try" ==) . fst) attrs
    then
      result `catchError` \case
        ExpansionNotBound {} -> return []
        e -> throwError e
    else result
  where
    result =
      case identify original of
        Just name
          | name `Set.notMember` validIds ->
              ifM (isJust <$> getExpansion @t name) nodes (throwNotBound name)
        _ -> nodes

mbAttrFilter :: Monad m => Filter m Attribute
mbAttrFilter (k, _) x
  | Just k' <- "?" `T.stripSuffix` k =
      first (const k') <<$>> x `catchError` \case
        ExpansionNotBound {} -> return []
        e -> throwError e
  | otherwise = x
