{-# LANGUAGE RankNTypes #-}

module Ondim.Extra.Exceptions
  ( tryAttrFilter,
  ) where

import Data.Text qualified as T
import Ondim

tryAttrFilter :: Monad m => Filter m Attribute
tryAttrFilter (k, _) x
  | Just k' <- "?" `T.stripSuffix` k =
      first (const k') <<$>> x `catchOndim` \_ _ _ _ -> return []
  | otherwise = x
