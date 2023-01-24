{-# LANGUAGE AllowAmbiguousTypes #-}

module Ondim.Extra.BindJSON
  ( bindObject,
    openObject,
    bindArray,
    listArray
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Map.Syntax (MapSyntaxM, (##))
import Ondim (Expansion, Ondim, OndimNode, OndimTag, binding, bindingText, liftChildren)
import Ondim.Extra.Expansions (prefixed)
import Relude.Extra (toPairs)

toTextual :: Value -> Maybe (Text, Text)
toTextual = \case
  String s -> Just (s, "string")
  Number n -> Just (show n, "number")
  Bool b -> Just (show b, "bool")
  Null -> Just ("", "null")
  _ -> Nothing

arrayTextMap :: Monad m => Text -> Array -> MapSyntaxM Text (m Text) ()
arrayTextMap name arr = prefixed name do
  ":type" ## pure "array"
  ":size" ## pure $ show $ length arr

arrayExpMap ::
  forall a tag m.
  (Monad m, OndimNode tag a, OndimTag tag) =>
  Text ->
  Array ->
  MapSyntaxM Text (Expansion tag m a) ()
arrayExpMap name arr = prefixed name do
  ":list" ## listArray arr

bindArray ::
  forall t tag m a.
  (Monad m, OndimTag tag, OndimNode tag t) =>
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
  (Monad m, OndimNode tag a, OndimTag tag) =>
  Array ->
  Expansion tag m a
listArray arr node =
  join <$> forM (toList arr) \el ->
    liftChildren node
      `bindingText` do
        whenJust (toTextual el) \(v, t) -> do
          "item" ## pure v
          "item:type" ## pure t
      & case el of
        Object o -> bindObject @a "item" o
        Array o -> bindArray @a "item" o
        _ -> id

objectTextMap :: Monad m => Text -> Object -> MapSyntaxM Text (m Text) ()
objectTextMap name obj = prefixed name do
  ":type" ## pure "object"
  ":size" ## pure $ show $ length obj

objectExpMap ::
  forall a tag m.
  (Monad m, OndimNode tag a, OndimTag tag) =>
  Text ->
  Object ->
  MapSyntaxM Text (Expansion tag m a) ()
objectExpMap name obj = prefixed name do
  ":open" ## openObject obj

bindObject ::
  forall t tag m a.
  (Monad m, OndimTag tag, OndimNode tag t) =>
  Text ->
  Object ->
  Ondim tag m a ->
  Ondim tag m a
bindObject name obj env =
  env
    `bindingText` objectTextMap name obj
    `binding` objectExpMap @t name obj

openObject ::
  forall a tag m.
  (Monad m, OndimNode tag a, OndimTag tag) =>
  Object ->
  Expansion tag m a
openObject obj node =
  liftChildren node
    `bindingText` do
      forM_ (toPairs obj) \(K.toText -> k, v) -> do
        case v of
          Array a -> arrayTextMap k a
          Object o -> objectTextMap k o
          _ -> whenJust (toTextual v) \(v', t) -> do
            k ## pure v'
            k <> ":type" ## pure t
    `binding` do
      forM_ (toPairs obj) \(K.toText -> k, v) -> do
        case v of
          Array a -> arrayExpMap @a k a
          Object o -> objectExpMap @a k o
          _ -> pure ()
