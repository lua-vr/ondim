{-# LANGUAGE RecordWildCards #-}
module Ondim.Targets.Whiskers.Load where

import Ondim.Extra.Loading (LoadConfig (..), LoadFn (..))
import Ondim.Targets.Whiskers.Parser (parseWhiskers)

loadWhiskers :: (Text, Text) -> LoadConfig
loadWhiskers d = LoadConfig {..}
  where
    patterns = ["**/*.w.*"]
    loadFn = LoadFn \fp bs -> parseWhiskers d fp $ decodeUtf8 bs
