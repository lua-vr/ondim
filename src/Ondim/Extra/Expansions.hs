-- | Useful library of expansions.
module Ondim.Extra.Expansions where

import Data.List qualified as L
import Data.Map qualified as Map
import Ondim
import Ondim.Debug

missingArgErr :: Text -> Ondim s a
missingArgErr key = throwTemplateError $ "Missing '" <> key <> "' argument."

ensureAttr ::
  Text ->
  [Attribute] ->
  Ondim s Text
ensureAttr key attrs = maybe (missingArgErr key) pure (L.lookup key attrs)

lookupAttr' ::
  (OndimNode t) =>
  Text ->
  t ->
  Ondim s Text
lookupAttr' key node = ensureAttr key =<< attributes node

ensureSingleAttr ::
  Text ->
  [Attribute] ->
  Ondim s Text
ensureSingleAttr key attrs =
  maybe (missingArgErr key) pure $
    L.lookup key attrs <|> viaNonEmpty (fst . head) attrs

lookupSingleAttr' ::
  (OndimNode t) =>
  Text ->
  t ->
  Ondim s Text
lookupSingleAttr' key node = ensureSingleAttr key =<< attributes node

identifiesAs :: (OndimNode t) => Text -> t -> Bool
identifiesAs n = (Just n ==) . identify

-- * Lists

listExp ::
  (a -> NamespaceItem s) ->
  [a] ->
  NamespaceMap s
listExp f list = do
  "size" #@ show $ length list
  unless (null list) $ "nonempty" #@ "true"
  "list" #* listList f list
  whenJust (viaNonEmpty head list) (("head" #:) . f)

listList ::
  forall a t s.
  (OndimNode t) =>
  (a -> NamespaceItem s) ->
  [a] ->
  Expansion s t
listList f list node = do
  alias <- fromMaybe "this" <$> lookupAttr "as" node
  expansion <- do
    expName <- lookupAttr "with" node
    case expName of
      Just name -> do
        exps <- getExpansion name
        either (throwExpFailure @t name) return exps
      Nothing -> return expandChildren
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
  (v -> NamespaceItem s) ->
  [(Text, v)] ->
  NamespaceMap s
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
  (v -> NamespaceItem s) ->
  Map Text v ->
  NamespaceMap s
mapExp vf obj = assocsExp vf (Map.toList obj)

-- * Booleans

ifElse ::
  (OndimNode t) =>
  Bool ->
  Expansion s t
ifElse cond node = do
  let els = children node
      yes = filter (not . identifiesAs "else") els
      no =
        maybe [] children $
          find (identifiesAs "else") els
  if cond
    then expandSubs yes
    else expandSubs no
{-# INLINEABLE ifElse #-}

-- * Text

switchWithDefault ::
  (OndimNode t) =>
  Maybe Text ->
  Expansion s t
switchWithDefault tag node = do
  let els = children node
  match <- (`findM` els) \x -> do
    if identifiesAs "case" x
      then do
        caseTag <- lookupSingleAttr' "id" x
        return $ Just caseTag == tag
      else return False
  fromMaybe (pure []) do
    child <- match <|> find (identifiesAs "default") els
    pure $ expandChildren child
  where
    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

renderExp ::
  forall a b s.
  ( HasCallStack,
    OndimNode a,
    OndimNode b
  ) =>
  (Text -> Either String b) ->
  Expansion s a
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
              x' <- expandSubs x
              let t = decodeUtf8 @Text (render x')
              return $ cast t
            Nothing -> noCast
        Nothing -> noRender
