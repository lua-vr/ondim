module Ondim.Targets.Pandoc.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Ondim
import Ondim.Extra.Expansions (ignore)
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
    ins InlineTpl name (loadPandoc inlineFromDocument -> tpl) s =
      s {expansions = insertExpansion name (someExpansion tpl) (expansions s)}
    ins BlockTpl name (loadPandoc blockFromDocument -> tpl) s =
      s {expansions = insertExpansion name (someExpansion tpl) (expansions s)}
    loadPandoc f txt =
      either
        (throw . TemplateLoadingException . toString . renderError)
        f
        ( runPure $
            readMarkdown
              def {readerExtensions = pandocExtensions}
              (decodeUtf8 txt :: Text)
        )

loadTemplates :: Monad n => [FilePath] -> IO (OndimState n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)

-- Template loading helpers

blockFromDocument :: Monad m => Pandoc -> Expansion m Block
blockFromDocument (Pandoc _ b) = fromTemplate b

inlineFromDocument :: Monad m => Pandoc -> Expansion m Inline
inlineFromDocument (Pandoc _ (Para i : _)) = fromTemplate i
inlineFromDocument _ = ignore
