{-# LANGUAGE RankNTypes #-}

module Ondim.Extra.BindJSON
  ( valueExpMap,
    arrayExpMap,
    objectExpMap,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Scientific
import Ondim
import Relude.Extra (toPairs)

valueExpMap :: Text -> Value -> ExpansionMap m
valueExpMap name = \case
  Object o -> objectExpMap name o
  Array a -> arrayExpMap name a
  String s -> name #@ s
  Number n -> name #@ prettyNum n
  Bool True -> name #@ "true"
  Bool False -> unbind name
  Null -> unbind name

arrayExpMap :: Text -> Array -> ExpansionMap m
arrayExpMap name arr =
  name #. do
    "size" #@ show $ length arr
    "list" #* listArray arr

listArray ::
  Array ->
  GlobalExpansion m
listArray arr node = do
  alias <- fromMaybe "item" <$> lookupAttr "as" node
  withExpansion alias Nothing $
    join <$> forM (toList arr) \el ->
      liftChildren node
        `binding` valueExpMap alias el

listObject :: Object -> GlobalExpansion m
listObject obj node = do
  alias <- fromMaybe "item" <$> lookupAttr "as" node
  withExpansion alias Nothing $
    join <$> forM (toPairs obj) \(K.toText -> k, el) ->
      liftChildren node
        `binding` do
          alias #. do
            "key" #@ k
            valueExpMap "value" el

objectExpMap ::
  Text ->
  Object ->
  ExpansionMap m
objectExpMap name obj =
  name #. do
    "type" #@ "object"
    "size" #@ show $ length obj
    "list" #* listObject obj
    forM_ (toPairs obj) (uncurry (valueExpMap . K.toText))

prettyNum :: Scientific -> Text
prettyNum x = case floatingOrInteger x of
  Left (r :: Float) -> show r
  Right (i :: Int) -> show i
