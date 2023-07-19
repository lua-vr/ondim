{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Aeson.Instances where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import Ondim

instance Conversible Array [Value] where
  convertTo = toList
  updateFrom _ = fromList

data ObjectLift

type ObjectSub = Custom Object ObjectLift

instance CanLift ObjectSub where
  liftSub (o :: Object) =
    KM.fromList <$> mapMaybeM go (KM.toList o)
    where
      go (k, v)
        | Just (k', '?') <- T.unsnoc (K.toText k) =
            (Just . (K.fromText k',) <$> liftSubstructures v)
              `catchFailure` \_ _ _ _ -> return Nothing
        | otherwise = Just . (k,) <$> liftSubstructures v

instance OndimNode Value where
  type
    ExpTypes Value =
      'SpecList
        '[ ToSpecSel ('ConsSel "Object") ObjectSub,
           ToSpecSel ('ConsSel "String") Text,
           ToSpecSel ('ConsSel "Array") (Converting Array Value)
         ]
  identify (Object o)
    | Just (String name) <- KM.lookup "$" o = Just name
  identify _ = Nothing
  children (Object o)
    | Just (Array a) <- KM.lookup "$args" o = toList a
  children _ = []
  attributes (Object o) = liftNodes $ KM.foldrWithKey go [] o
    where
      go k (String t) a = (K.toText k, t) : a
      go _ _ a = a
  attributes _ = pure []
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . String
    | otherwise = Nothing
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just \case
        String t -> [t]
        _notString -> mempty
    | Just Refl <- eqT @t @Rendered = Just $ one . encode
    | otherwise = Nothing
