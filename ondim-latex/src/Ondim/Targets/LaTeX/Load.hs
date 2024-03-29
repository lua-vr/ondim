{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.LaTeX.Load where

import Ondim.Loading (LoadConfig (..), loadFnSimple)
import Ondim.Targets.LaTeX.Expansions (defaultState)
import Ondim.Targets.LaTeX.Parser (parseLaTeX)

loadLaTeX :: LoadConfig s
loadLaTeX = LoadConfig {..}
  where
    patterns = ["**/*.tex"]
    initialState = defaultState
    loadFn = loadFnSimple \fp bs -> parseLaTeX fp $ decodeUtf8 bs
