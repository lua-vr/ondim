{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.Whiskers.Load where

import Ondim.Extra.Loading (LoadConfig (..), LoadFn (..))
import Ondim.Targets.Whiskers.Expansions (defaultState)
import Ondim.Targets.Whiskers.Parser (parseWhiskers)

loadWhiskers :: Monad m => (Text, Text) -> LoadConfig m
loadWhiskers d = LoadConfig {..}
  where
    patterns = ["**/*.w.*"]
    initialState = defaultState
    loadFn = LoadFn \fp bs -> parseWhiskers d fp $ decodeUtf8 bs
