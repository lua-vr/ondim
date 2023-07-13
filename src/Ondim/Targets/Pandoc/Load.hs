{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.Pandoc.Load where

import Ondim.Extra.Loading (LoadConfig (..), LoadFn (..))
import Ondim.Targets.Pandoc.Instances ()
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Error (renderError)
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Options (def, readerExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)

loadPandocMd :: LoadConfig
loadPandocMd = LoadConfig {..}
  where
    patterns = ["**/*.md"]
    loadFn = LoadFn \_ bs ->
      first (toString . renderError) $
        runPure $
          readMarkdown def {readerExtensions = pandocExtensions} (decodeUtf8 bs :: Text)
