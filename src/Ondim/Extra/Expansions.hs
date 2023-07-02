{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Useful library of expansions.
module Ondim.Extra.Expansions where

import Data.List qualified as L
import Data.Map qualified as Map
import Ondim

lookupAttr' ::
  (Monad m, OndimNode t) =>
  Text ->
  t ->
  Ondim m Text
lookupAttr' key node =
  maybe (throwTemplateError $ "Missing '" <> key <> "' argument.") pure
    . L.lookup key
    =<< attributes node

getSingleAttr :: Text -> [Attribute] -> Maybe Text
getSingleAttr name attrs = L.lookup name attrs <|> viaNonEmpty (fst . head) attrs

getSingleAttr' ::
  (Monad m, OndimNode t) =>
  Text ->
  t ->
  Ondim m Text
getSingleAttr' name node =
  maybe (throwTemplateError $ "Missing '" <> name <> "' argument") pure
    . getSingleAttr name
    =<< attributes node

identifiesAs :: OndimNode t => [Text] -> t -> Bool
identifiesAs n = (Just n ==) . fmap splitExpansionKey . identify

-- * Lists

listExp ::
  Monad m =>
  (a -> SomeExpansion m) ->
  [a] ->
  ExpansionMap m
listExp f list = do
  "size" #@ show $ length list
  unless (null list) $ "nonempty" #@ "true"
  "list" #* listList f list
  whenJust (viaNonEmpty head list) (("head" #:) . f)

listList ::
  forall a m t.
  GlobalConstraints m t =>
  (a -> SomeExpansion m) ->
  [a] ->
  Expansion m t
listList f list node = do
  alias <- fromMaybe "this" <$> lookupAttr "as" node
  intercalateWith <- lookupAttr "intercalate" node
  let inter txt
        | Just ft <- fromText @t = intercalate (ft txt)
        | otherwise = join
      join' = maybe join inter intercalateWith
  withSomeExpansion alias Nothing $
    join' <$> forM list \el ->
      liftChildren node
        `binding` do alias #: f el

-- * Assocs and maps

assocsExp ::
  Monad m =>
  (v -> SomeExpansion m) ->
  [(Text, v)] ->
  ExpansionMap m
assocsExp vf obj = do
  "size" #@ show $ length obj
  unless (null obj) $ "nonempty" #@ "true"
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
  Monad m =>
  (v -> SomeExpansion m) ->
  Map Text v ->
  ExpansionMap m
mapExp vf obj = assocsExp vf (Map.toList obj)

-- * Booleans

ifElse ::
  forall t m.
  ( OndimNode t,
    Monad m
  ) =>
  Bool ->
  Expansion m t
ifElse cond node = do
  let els = children node
      yes = filter (not . identifiesAs ["else"]) els
      no =
        maybe [] children $
          find (identifiesAs ["else"]) els
  if cond
    then liftNodes yes
    else liftNodes no
{-# INLINEABLE ifElse #-}

-- * Text

switchWithDefault ::
  forall m t.
  GlobalConstraints m t =>
  Text ->
  Expansion m t
switchWithDefault tag node = do
  let els = children node
  match <- (`findM` els) \x -> do
    caseTag <- getSingleAttr "tag" <$> attributes x
    return $ identifiesAs ["case"] x && caseTag == Just tag
  fromMaybe (pure []) do
    child <- match <|> find (identifiesAs ["default"]) els
    pure $ liftChildren child
  where
    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)
