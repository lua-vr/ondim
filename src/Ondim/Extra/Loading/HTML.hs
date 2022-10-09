module Ondim.Extra.Loading.HTML where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, NoLoggingT (runNoLoggingT))
import Ondim
import Ondim.Extra.Loading
import Ondim.HTML
import Relude.Extra (delete, insert, (%~))
import Text.XmlHtml qualified as X

loadTemplatesDynamic ::
  forall m n.
  (Monad n, MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  [FilePath] ->
  m (OndimMS (HtmlTag n), (OndimMS (HtmlTag n) -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins del
  where
    patts = [((), "**/*.tpl")]
    ins () name text =
      let template =
            either
              (throw . TemplateLoadingException)
              (fromDocument name)
              (X.parseHTML (toString name) text)
       in ondimState
            %~ (\s -> s {expansions = insert name template (expansions s)})
    del () name =
      ondimState @HtmlNode @(HtmlTag n)
        %~ (\s -> s {expansions = delete name (expansions s)})

loadTemplates :: Monad n => [FilePath] -> IO (OndimMS (HtmlTag n))
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)
