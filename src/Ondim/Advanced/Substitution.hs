{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Ondim.Advanced.Substitution
  ( SubstConfig (..),
    SText,
    SAttr,
    SAttrs,
    getSAttributes,
    SAttrData (..),
  ) where

import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (..))
import GHC.TypeLits (KnownChar, charVal)
import Ondim
import Ondim.Advanced
import Ondim.Debug

data SubstConfig
  = SubstConfig
      -- | Start
      Char
      -- | Opening delimiter
      Char
      -- | Closing delimiter
      Char

class (Typeable c) => KnownConfig (c :: SubstConfig) where
  substConfigVal :: Proxy c -> (Char, Char, Char)

instance (KnownChar a, KnownChar b, KnownChar c) => KnownConfig ('SubstConfig a b c) where
  substConfigVal _ = (charVal (Proxy @a), charVal (Proxy @b), charVal (Proxy @c))

data SubstLift (c :: SubstConfig)

type SText (c :: SubstConfig) = Custom Text (SubstLift c)

instance (KnownConfig c) => Expansible (SText c) where
  expandSpec (t :: Text) = go t
    where
      (s, a, b) = substConfigVal (Proxy @c)
      go text = do
        let (beg, rest0) = T.break (== s) text
        (beg <>) <$> case T.uncons rest0 of
          Just (_, rest1)
            | Just (c0, rest2) <- T.uncons rest1,
              c0 == a,
              let (name, rest3) = T.break (== b) rest2,
              Just (_, rest4) <- T.uncons rest3 -> do
                res <- callText name
                (res <>) <$> go rest4
            | otherwise -> T.cons s <$> go rest1
          _noStartChar -> return mempty

newtype SAttrData (c :: SubstConfig) = SAttrData {unSAttrData :: (Text, Text)}
  deriving (Generic)

instance Conversible (Text, Text) (SAttrData c) where
  updateFrom _ = coerce
  convertTo = coerce

-- Could've used MatchWith here, but it would throw errors because users cannot import the constructor.
type SAttr (c :: SubstConfig) = Converting (Text, Text) (Nesting (SAttrData c))

instance (KnownConfig c) => OndimNode (SAttrData c) where
  type
    ExpTypes (SAttrData c) =
      'SpecList
        '[ ToSpec
            (ModSub (Text, Text) '[ToSpec (SText c)])
         ]
  type NodeListSpec (SAttrData c) = LiftSAttrsWithTry' c
  identify (SAttrData (name, _)) = T.stripPrefix "e:" name
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @(Text, Text) = Just $ one . coerce
    | otherwise = Nothing

instance Conversible [(Text, Text)] [SAttrData c] where
  updateFrom _ = coerce
  convertTo = coerce

-- Could've used MatchWith here, but it would throw errors because users cannot import the constructor.
type SAttrs (c :: SubstConfig) = Converting [(Text, Text)] (NL (SAttrData c))

type LiftSAttrsWithTry' (c :: SubstConfig) = Custom [SAttrData c] (SubstLift c)

instance (KnownConfig c) => Expansible (LiftSAttrsWithTry' c) where
  expandSpec = foldMapM go
    where
      go x@(coerce -> (k, v :: Text))
        | Just (k', '?') <- T.unsnoc k =
            expandNode @(SAttrData c) (coerce (k', v))
              `catchFailure` \_ _ _ _ -> return []
        | otherwise = expandNode x

getSAttributes ::
  forall c s t.
  ( KnownConfig c,
    AllMods (Substructure (SAttrData c)) (ExpTypes t),
    OndimNode t
  ) =>
  t ->
  Ondim s [(Text, Text)]
getSAttributes = fmap coerce . expandNodes . getSubstructure @(SAttrData c)
