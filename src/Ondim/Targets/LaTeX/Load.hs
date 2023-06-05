module Ondim.Targets.LaTeX.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Ondim
import Ondim.Extra.Loading
import Ondim.Targets.LaTeX.Parser (parseLaTeX)

loadTemplatesDynamic ::
  forall m n.
  (Monad n, MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins
  where
    patts = [((), "**/*.tex")]
    ins () fp text =
      either
        (throw . TemplateLoadingException)
        (fromTemplate $ FileDefinition fp)
        (parseLaTeX fp $ decodeUtf8 text)

loadTemplates :: Monad n => [FilePath] -> IO (OndimState n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)
