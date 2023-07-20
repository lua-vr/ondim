{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.LaTeX.Load where

import Ondim.Extra.Loading (LoadConfig (..), LoadFn (..))
import Ondim.Targets.LaTeX.Expansions (defaultState)
import Ondim.Targets.LaTeX.Parser (parseLaTeX)

loadLaTeX :: Monad m => LoadConfig m
loadLaTeX = LoadConfig {..}
  where
    patterns = ["**/*.tex"]
    initialState = defaultState
    loadFn = LoadFn \fp bs -> parseLaTeX fp $ decodeUtf8 bs
