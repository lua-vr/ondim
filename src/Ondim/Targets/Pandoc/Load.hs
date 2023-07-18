{-# LANGUAGE RecordWildCards #-}

module Ondim.Targets.Pandoc.Load where

import Ondim.Extra.Loading (LoadConfig (..), LoadFn (..))
import Ondim.Targets.Pandoc.Instances ()
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Error (renderError)
import Text.Pandoc.Extensions (Extension (..), disableExtensions, extensionsFromList, pandocExtensions)
import Text.Pandoc.Options (def, readerExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)

loadPandocMd :: LoadConfig
loadPandocMd = LoadConfig {..}
  where
    patterns = ["**/*.md"]
    ext =
      disableExtensions pandocExtensions $
        extensionsFromList
          [ Ext_auto_identifiers
          ]
    loadFn = LoadFn \_ bs ->
      first (toString . renderError) $
        runPure $
          readMarkdown def {readerExtensions = ext} (decodeUtf8 bs :: Text)
