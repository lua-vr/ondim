module Ondim.Targets.HTML.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, NoLoggingT (runNoLoggingT))
import Ondim
import Ondim.Extra.Loading
import Ondim.Targets.HTML.Instances
import Text.XmlHtml qualified as X

loadTemplatesDynamic ::
  forall m n.
  (Monad n, MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins
  where
    patts = [((), "**/*.html")]
    ins () fp text =
      either
        (throw . TemplateLoadingException)
        (fromDocument $ FileDefinition fp)
        (X.parseHTML fp text)

loadTemplates :: Monad n => [FilePath] -> IO (OndimState n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)

-- * Template loading helpers

fromDocument :: Monad m => DefinitionSite -> X.Document -> SomeExpansion m
fromDocument site = templateData' site . fromNodeList . X.docContent
