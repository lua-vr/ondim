module Ondim.Targets.Pandoc.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Data.HashMap.Strict (insert)
import Ondim
import Ondim.Extra.Expansions (ignore)
import Ondim.Extra.Loading
import Ondim.MultiWalk.Core (toSomeExpansion)
import Ondim.Targets.Pandoc.Instances
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
  m (OndimState PandocTag n, (OndimState PandocTag n -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins
  where
    patts = [(InlineTpl, "**/*.inl.md"), (BlockTpl, "**/*.blk.md")]
    ins InlineTpl name (loadPandoc (inlineFromDocument name) -> tpl) s =
      s {expansions = insert name (toSomeExpansion tpl) (expansions s)}
    ins BlockTpl name (loadPandoc (blockFromDocument name) -> tpl) s =
      s {expansions = insert name (toSomeExpansion tpl) (expansions s)}
    loadPandoc f txt =
      either
        (throw . TemplateLoadingException . toString . renderError)
        f
        ( runPure $
            readMarkdown
              def {readerExtensions = pandocExtensions}
              (decodeUtf8 txt :: Text)
        )

loadTemplates :: Monad n => [FilePath] -> IO (OndimState PandocTag n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)

-- Template loading helpers

blockFromDocument :: Monad m => Text -> Pandoc -> Expansion PandocTag m Block
blockFromDocument name (Pandoc _ b) = fromTemplate name b

inlineFromDocument :: Monad m => Text -> Pandoc -> Expansion PandocTag m Inline
inlineFromDocument name (Pandoc _ (Para i : _)) = fromTemplate name i
inlineFromDocument _ _ = ignore
