module Ondim.Targets.Whiskers.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Ondim
import Ondim.Extra.Loading
import Ondim.Targets.Whiskers.Parser (parseWhiskers)

loadTemplatesDynamic ::
  forall m n.
  (Monad n, MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  (Text, Text) ->
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic d =
  loadTemplatesDynamic' patts ins
  where
    patts = [((), "**/*.w.*")]
    ins () name text s =
      let template =
            either
              (throw . TemplateLoadingException)
              fromTemplate
              (parseWhiskers d (toString name) $ decodeUtf8 text)
       in s {expansions = insertExpansion name (toSomeExpansion template) (expansions s)}

loadTemplates :: Monad n => (Text, Text) -> [FilePath] -> IO (OndimState n)
loadTemplates d dirs = fst <$> runNoLoggingT (loadTemplatesDynamic d dirs)
