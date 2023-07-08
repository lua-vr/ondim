module Ondim.Targets.Pandoc.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Ondim
import Ondim.Extra.Loading
import Ondim.Targets.Pandoc.Instances ()
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Error (renderError)
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Options (def, readerExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)

loadTemplatesDynamic ::
  forall m n.
  (Monad n, MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins
  where
    patts = [((), "**/*.md")]
    ins () fp txt =
      either
        (throw . TemplateLoadingException . toString . renderError)
        (templateData' (FileDefinition fp))
        $ runPure
        $ readMarkdown
          def {readerExtensions = pandocExtensions}
          (decodeUtf8 txt :: Text)

loadTemplates :: Monad n => [FilePath] -> IO (OndimState n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)
