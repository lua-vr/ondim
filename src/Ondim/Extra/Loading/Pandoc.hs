module Ondim.Extra.Loading.Pandoc where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Ondim
import Ondim.Extra.Loading
import Ondim.Pandoc
import Relude.Extra (delete, insert, (%~))
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
  m (OndimMS PandocTag n, (OndimMS PandocTag n -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins del
  where
    patts = [(InlineTpl, "**/*.inl.md"), (BlockTpl, "**/*.blk.md")]
    ins InlineTpl name (loadPandoc (inlineFromDocument name) -> tpl) =
      ondimState
        %~ (\s -> s {expansions = insert name tpl (expansions s)})
    ins BlockTpl name (loadPandoc (blockFromDocument name) -> tpl) =
      ondimState
        %~ (\s -> s {expansions = insert name tpl (expansions s)})
    del InlineTpl name =
      ondimState @PandocTag @n @Inline
        %~ (\s -> s {expansions = delete name (expansions s)})
    del BlockTpl name =
      ondimState @PandocTag @n @Block
        %~ (\s -> s {expansions = delete name (expansions s)})
    loadPandoc f txt =
      either
        (throw . TemplateLoadingException . toString . renderError)
        f
        ( runPure $
            readMarkdown
              def {readerExtensions = pandocExtensions}
              (decodeUtf8 txt :: Text)
        )

loadTemplates :: Monad n => [FilePath] -> IO (OndimMS PandocTag n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)
