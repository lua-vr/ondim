{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.HTML.Load where

import Ondim.Extra.Loading (LoadConfig (..), LoadFn (..))
import Ondim.Targets.HTML.Instances
import Text.HTML.DOM qualified as HTML

loadHtml :: LoadConfig
loadHtml = LoadConfig {..}
  where
    patterns = ["**/*.html"]
    loadFn = LoadFn \_ bs -> Right $ toHtmlDocument (HTML.parseLBS bs)
