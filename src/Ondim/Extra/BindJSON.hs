{-# LANGUAGE AllowAmbiguousTypes #-}

module Ondim.Extra.BindJSON
  ( bindObject,
    openObject,
    bindArray,
    listArray,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Map.Syntax (MapSyntaxM, (##))
import Data.Scientific
import Ondim (Expansion, Ondim, binding, bindingText, liftChildren)
import Ondim.Extra.Expansions (HasAttrChild, lookupAttr, prefixed)
import Relude.Extra (toPairs)

toTextual :: Value -> Maybe (Text, Text)
toTextual = \case
  String s -> Just (s, "string")
  Number n -> Just (prettyNum n, "number")
  Bool b -> Just (show b, "bool")
  Null -> Just ("", "null")
  _ -> Nothing

arrayTextMap :: Monad m => Text -> Array -> MapSyntaxM Text (m Text) ()
arrayTextMap name arr = prefixed name do
  ":type" ## pure "array"
  ":size" ## pure $ show $ length arr

arrayExpMap ::
  forall a tag m.
  (Monad m, HasAttrChild tag a) =>
  Text ->
  Array ->
  MapSyntaxM Text (Expansion tag m a) ()
arrayExpMap name arr = prefixed name do
  ":list" ## listArray arr

bindArray ::
  forall t tag m a.
  (Monad m, HasAttrChild tag t) =>
  Text ->
  Array ->
  Ondim tag m a ->
  Ondim tag m a
bindArray name arr env =
  env
    `bindingText` arrayTextMap name arr
    `binding` arrayExpMap @t name arr

listArray ::
  forall a tag m.
  (Monad m, HasAttrChild tag a) =>
  Array ->
  Expansion tag m a
listArray arr node = do
  alias <- fromMaybe "item" <$> lookupAttr "as" node
  join <$> forM (toList arr) \el ->
    liftChildren node
      `bindingText` do
        whenJust (toTextual el) \(v, t) -> do
          alias ## pure v
          alias <> ":type" ## pure t
      & case el of
        Object o -> bindObject @a alias o
        Array o -> bindArray @a alias o
        _ -> id

objectTextMap :: Monad m => Text -> Object -> MapSyntaxM Text (m Text) ()
objectTextMap name obj = prefixed name do
  ":type" ## pure "object"
  ":size" ## pure $ show $ length obj

objectExpMap ::
  forall a tag m.
  (Monad m, HasAttrChild tag a) =>
  Text ->
  Object ->
  MapSyntaxM Text (Expansion tag m a) ()
objectExpMap name obj = prefixed name do
  ":open" ## \node -> do
    pfx <- lookupAttr "prefix" node
    openObject @a pfx obj $ liftChildren node

bindObject ::
  forall t tag m a.
  (Monad m, HasAttrChild tag t) =>
  Text ->
  Object ->
  Ondim tag m a ->
  Ondim tag m a
bindObject name obj env =
  env
    `bindingText` objectTextMap name obj
    `binding` objectExpMap @t name obj

openObject ::
  forall t a tag m.
  (Monad m, HasAttrChild tag t) =>
  Maybe Text ->
  Object ->
  Ondim tag m a ->
  Ondim tag m a
openObject (maybe "" (<> ":") -> pfx) obj node =
  node
    `bindingText` prefixed pfx do
      forM_ (toPairs obj) \(K.toText -> k, v) -> do
        case v of
          Array a -> arrayTextMap k a
          Object o -> objectTextMap k o
          _ -> whenJust (toTextual v) \(v', t) -> do
            k ## pure v'
            k <> ":type" ## pure t
    `binding` prefixed pfx do
      forM_ (toPairs obj) \(K.toText -> k, v) -> do
        case v of
          Array a -> arrayExpMap @t k a
          Object o -> objectExpMap @t k o
          _ -> pure ()

prettyNum :: Scientific -> Text
prettyNum x = case floatingOrInteger x of
  Left (r :: Float) -> show r
  Right (i :: Int) -> show i
