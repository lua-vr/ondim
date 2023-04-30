{-# LANGUAGE RankNTypes #-}

module Ondim.Extra.Exceptions where

import Data.Text qualified as T
import Ondim

notBoundFilter :: forall t m. (Monad m, OndimNode t) => (Text -> Bool) -> Filter m t
notBoundFilter validId (original :: t) nodes = do
  let attrs =
        fromRight [] $
          runIdentity $
            evalOndimT $
              attributes original
  if any (("@try" ==) . fst) attrs
    then
      result `catchOndim` \case
        OndimException (ExpansionNotBound {}) _ -> return (Just [])
        _ -> return Nothing
    else result
  where
    result =
      case identify original of
        Just name
          | not (validId name) ->
              ifM (isJust <$> getExpansion @t name) nodes (throwNotBound @t name)
        _ -> nodes

mbAttrFilter :: Monad m => Filter m Attribute
mbAttrFilter (k, _) x
  | Just k' <- "?" `T.stripSuffix` k =
      first (const k') <<$>> x `catchOndim` \case
        OndimException (ExpansionNotBound {}) _ -> return (Just [])
        _ -> return Nothing
  | otherwise = x
