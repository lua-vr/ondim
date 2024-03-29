{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.Whiskers.Load where

import Ondim.Loading (LoadConfig (..), loadFnSimple)
import Ondim.Targets.Whiskers.Expansions (defaultState)
import Ondim.Targets.Whiskers.Parser (parseWhiskers)

loadWhiskers :: (Text, Text) -> LoadConfig s
loadWhiskers d = LoadConfig {..}
  where
    patterns = ["**/*.w.*"]
    initialState = defaultState
    loadFn = loadFnSimple \fp bs -> parseWhiskers d fp $ decodeUtf8 bs
