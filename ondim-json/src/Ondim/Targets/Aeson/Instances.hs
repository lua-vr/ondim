{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Aeson.Instances where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Ondim
import Ondim.Advanced
import Ondim.Debug
import Ondim.Advanced.Substitution (SAttr, SAttrs, SText, SubstConfig (..))

type ASConfig = 'SubstConfig '$' '{' '}'
type AesonText = SText ASConfig
type AesonAttr = SAttr ASConfig
type AesonAttrs = SAttrs ASConfig

instance Conversible Array [Value] where
  convertTo = toList
  updateFrom _ = fromList

data ObjectLift

type ObjectSub = Custom Object ObjectLift

instance Expansible ObjectSub where
  expandSpec (o :: Object) =
    KM.fromList <$> mapMaybeM go (KM.toList o)
    where
      go (k, v)
        | Just (k', '?') <- T.unsnoc (K.toText k) =
            (Just . (K.fromText k',) <$> expandSubstructures v)
              `catchFailure` \_ _ _ _ -> return Nothing
        | otherwise = Just . (k,) <$> expandSubstructures v

instance OndimNode Value where
  type
    ExpTypes Value =
      'SpecList
        '[ ToSpecSel ('ConsSel "Object") ObjectSub,
           ToSpecSel ('ConsSel "String") AesonText,
           ToSpecSel ('ConsSel "Array") (Converting Array (NL Value))
         ]
  identify (Object o)
    | Just (String name) <- KM.lookup "$" o = Just name
  identify _ = Nothing
  children (Object o)
    | Just (Array (toList -> a)) <- KM.lookup "$args" o = a
  children _ = mempty
  attributes (Object o) = expandSpec @AesonAttrs $ KM.foldrWithKey go [] o
    where
      go k (String t) a = (K.toText k, t) : a
      go _ _ a = a
  attributes _ = pure []
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . String
    | otherwise = Nothing
  nodeAsText = Just \case
    String t -> t
    _notString -> mempty
  renderNode = Just encode
