{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Aeson.Instances where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Ondim

instance Conversible Object [Value] where
  convertTo m =
    case KM.lookup "$args" m of
      Just (Array a) -> coerce $ toList a
      _notArray -> []
  updateFrom m values =
    if null values
      then m
      else KM.insert "$args" (Array $ fromList $ coerce values) m

instance Conversible Object [Attribute] where
  convertTo = KM.foldrWithKey go []
    where
      go k (String t) a = (K.toText k, t) : a
      go _ _ a = a
  updateFrom m attrs = fromList (map go attrs) <> m
    where
      go (k, v) = (K.fromText k, String v)

instance Conversible Array [Value] where
  convertTo = coerce . toList
  updateFrom _ = fromList . coerce

instance OndimNode Value where
  type
    ExpTypes Value =
      '[ ToSpecSel ('ConsSel "Object") (Converting Object Value),
         ToSpecSel ('ConsSel "Object") (Converting Object Attribute),
         ToSpecSel ('ConsSel "String") Text,
         ToSpecSel ('ConsSel "Array") (Converting Array Value)
       ]
  identify (Object o)
    | Just (String name) <- KM.lookup "$" o = Just name
  identify _ = Nothing
  fromText = Just $ one . String
