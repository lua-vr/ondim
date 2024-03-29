{-# LANGUAGE RankNTypes #-}

module Ondim.Extra.BindJSON
  ( valueExp,
    arrayExp,
    objectExp,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Scientific
import Ondim
import Ondim.Extra.Expansions (listExp, assocsExp)

valueExp :: Value -> NamespaceItem s
valueExp = \case
  Object o -> namespace $ objectExp o
  Array a -> namespace $ arrayExp a
  String s -> textData s
  Number n -> textData (prettyNum n)
  Bool True -> textData "true"
  Bool False -> textData "false"
  Null -> textData "null"

arrayExp :: Array -> NamespaceMap s
arrayExp arr = listExp valueExp (toList arr)

objectExp ::
  Object ->
  NamespaceMap s
objectExp obj = assocsExp valueExp (map (first K.toText) $ KM.toList obj)

prettyNum :: Scientific -> Text
prettyNum x = case floatingOrInteger x of
  Left (r :: Float) -> show r
  Right (i :: Int) -> show i
