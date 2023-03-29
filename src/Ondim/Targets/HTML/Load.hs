module Ondim.Targets.HTML.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, NoLoggingT (runNoLoggingT))
import Data.HashMap.Strict (insert)
import Ondim
import Ondim.Extra.Loading
import Ondim.MultiWalk.Core (toSomeExpansion)
import Ondim.Targets.HTML.Instances
import Text.XmlHtml qualified as X

loadTemplatesDynamic ::
  forall m n.
  (Monad n, MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  [FilePath] ->
  m (OndimState HtmlTag n, (OndimState HtmlTag n -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins
  where
    patts = [((), "**/*.tpl")]
    ins () name text s =
      let template =
            either
              (throw . TemplateLoadingException)
              (fromDocument name)
              (X.parseHTML (toString name) text)
       in s {expansions = insert name (toSomeExpansion template) (expansions s)}

loadTemplates :: Monad n => [FilePath] -> IO (OndimState HtmlTag n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)

-- * Template loading helpers

fromDocument :: Monad m => Text -> X.Document -> Expansion HtmlTag m HtmlNode
fromDocument name = fromTemplate name . fromNodeList . X.docContent
