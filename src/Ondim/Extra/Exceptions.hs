{-# LANGUAGE RankNTypes #-}

module Ondim.Extra.Exceptions
  ( tryFilter,
    tryAttrFilter,
  ) where

import Data.Text qualified as T
import Ondim

tryFilter :: forall t m. (Monad m, OndimNode t) => Filter m t
tryFilter original nodes = do
  let attrs =
        fromRight [] $
          runIdentity $
            evalOndimT $
              attributes original
  if any (("@try" ==) . fst) attrs
    then nodes `catchOndim` \_ _ _ _ -> return []
    else nodes

tryAttrFilter :: Monad m => Filter m Attribute
tryAttrFilter (k, _) x
  | Just k' <- "?" `T.stripSuffix` k =
      first (const k') <<$>> x `catchOndim` \_ _ _ _ -> return []
  | otherwise = x
