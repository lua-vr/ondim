{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.HTML.Load (loadHtml) where

import Ondim.Extra.Loading (LoadConfig (..))
import Ondim.Targets.HTML.Expansions (defaultState)
import Ondim.Targets.HTML.Instances
import Ondim.Targets.HTML.Parser (parseLBS)
import Ondim.Targets.Whiskers.Load (loadFnWhiskers)

loadHtml :: Monad m => LoadConfig m
loadHtml = LoadConfig {..}
  where
    initialState = defaultState
    patterns = ["**/*.html"]
    loadFn = loadFnWhiskers ("${", "}") \_ bs -> Right $ toHtmlDocument (parseLBS bs)
