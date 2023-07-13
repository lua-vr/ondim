{-# LANGUAGE RecordWildCards #-}
module Ondim.Targets.LaTeX.Load where

import Ondim.Extra.Loading (LoadConfig (..), LoadFn (..))
import Ondim.Targets.LaTeX.Parser (parseLaTeX)

loadLaTeX :: LoadConfig
loadLaTeX = LoadConfig {..}
  where
    patterns = ["**/*.tex"]
    loadFn = LoadFn \fp bs -> parseLaTeX fp $ decodeUtf8 bs
