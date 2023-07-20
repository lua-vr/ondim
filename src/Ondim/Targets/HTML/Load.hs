{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.HTML.Load (loadHtml) where

import Ondim.Extra.Loading (LoadConfig (..), LoadFn (..))
import Ondim.Targets.HTML.Expansions (defaultState)
import Ondim.Targets.HTML.Instances
import Ondim.Targets.HTML.Parser (parseLBS)

loadHtml :: Monad m => LoadConfig m
loadHtml = LoadConfig {..}
  where
    initialState = defaultState
    patterns = ["**/*.html"]
    loadFn = LoadFn \_ bs -> Right $ toHtmlDocument (parseLBS bs)
