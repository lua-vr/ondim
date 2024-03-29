{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.HTML.Load (loadHtml) where

import Ondim.Loading (LoadConfig (..), loadFnSimple)
import Ondim.Targets.HTML.Expansions (defaultState)
import Ondim.Targets.HTML.Instances
import Ondim.Targets.HTML.Parser (parseLBS)

loadHtml :: LoadConfig s
loadHtml = LoadConfig {..}
  where
    initialState = defaultState
    patterns = ["**/*.html"]
    loadFn = loadFnSimple \_ bs -> Right $ toHtmlDocument (parseLBS bs)
