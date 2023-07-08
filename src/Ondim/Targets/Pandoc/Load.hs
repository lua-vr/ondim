module Ondim.Targets.Pandoc.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Ondim
import Ondim.Extra.Standard (ignore)
import Ondim.Extra.Loading
import Ondim.Targets.Pandoc.Instances ()
import Text.Pandoc (def, pandocExtensions, readMarkdown, readerExtensions, renderError, runPure)
import Text.Pandoc.Definition

data TemplateType
  = InlineTpl
  | BlockTpl
  -- ...add more?
  deriving (Eq, Ord)

loadTemplatesDynamic ::
  forall m n.
  (Monad n, MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins
  where
    patts = [(InlineTpl, "**/*.inl.md"), (BlockTpl, "**/*.blk.md")]
    ins t fp txt =
      either
        (throw . TemplateLoadingException . toString . renderError)
        (fromDocument t (FileDefinition fp))
        $ runPure
        $ readMarkdown
          def {readerExtensions = pandocExtensions}
          (decodeUtf8 txt :: Text)

loadTemplates :: Monad n => [FilePath] -> IO (OndimState n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)

-- Template loading helpers

fromDocument :: Monad m => TemplateType -> DefinitionSite -> Pandoc -> SomeExpansion m
fromDocument BlockTpl site (Pandoc _ b) = templateData' site b
fromDocument InlineTpl site (Pandoc _ (Para i : _)) = templateData' site i
fromDocument InlineTpl site _ = someExpansion' @Inline site ignore
