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
  (OndimNode t, Monad m) =>
  (a -> SomeExpansion m) ->
  [a] ->
  Expansion m t
listList f list node = do
  alias <- fromMaybe "this" <$> lookupAttr "as" node
  expansion <- do
    expName <- lookupAttr "with" node
    case expName of
      Just name -> do
        exps <- getExpansion name
        either (throwExpFailure @t name) return exps
      Nothing -> return liftChildren
  intercalateWith <- lookupAttr "intercalate" node
  let inter txt
        | Just cast <- ondimCast = intercalate (cast txt)
        | otherwise = join
      join' = maybe join inter intercalateWith
  withSomeExpansion alias Nothing $
    join' <$> forM list \el ->
      expansion node
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
  (OndimNode t, Monad m) =>
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
  (OndimNode t, Monad m) =>
  Maybe Text ->
  Expansion m t
switchWithDefault tag node = do
  let els = children node
  match <- (`findM` els) \x -> do
    if identifiesAs ["case"] x
      then do
        caseTag <- getSingleAttr' "id" x
        return $ Just caseTag == tag
      else return False
  fromMaybe (pure []) do
    child <- match <|> find (identifiesAs ["default"]) els
    pure $ liftChildren child
  where
    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

renderExp ::
  forall m a b.
  ( HasCallStack,
    OndimNode a,
    OndimNode b,
    Monad m
  ) =>
  (Text -> Either String b) ->
  Expansion m a
renderExp f node = do
  parsed <- f <$> lookupAttr' "text" node
  either (throwTemplateError . toText) convert parsed
  where
    noRender = throwTemplateError "source is missing cast to rendered!"
    noCast = throwTemplateError "target is missing cast from text!"
    convert x = do
      case renderNode of
        Just render ->
          case ondimCast of
            Just cast -> do
              x' <- liftSubstructures x
              let t = decodeUtf8 @Text (render x')
              return $ cast t
            Nothing -> noCast
        Nothing -> noRender
