module Ondim.Extra.Loading where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Ondim
import Relude.Extra (minimumOn1, toPairs)
import System.FilePath (dropExtensions, splitDirectories, (</>))
import System.FilePattern (FilePattern)
import System.UnionMount

newtype TemplateLoadingError = TemplateLoadingException String
  deriving (Eq, Show)
  deriving anyclass (Exception)

loadTemplatesDynamic' ::
  forall m n tplTypes tag.
  (OndimTag tag, Ord tplTypes) =>
  (MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  -- | Patterns to look for.
  [(tplTypes, FilePattern)] ->
  -- | Insertion
  (tplTypes -> Text -> ByteString -> OndimState tag n -> OndimState tag n) ->
  -- | Places to look for templates, in descending order of priority.
  [FilePath] ->
  m (OndimState tag n, (OndimState tag n -> m ()) -> m ())
loadTemplatesDynamic' patts ins places =
  let sources = fromList (zip (zip [1 ..] places) places)
      patterns = patts
      exclude = []
      initial = mempty
      handler :: Change (Int, FilePath) tplTypes -> m (OndimState tag n -> OndimState tag n)
      handler chg =
        appEndo . mconcat . coerce . join
          <$> forM (toPairs chg) \(tplType, chg') ->
            forM (toPairs chg') \(file, fa) ->
              let name =
                    fromString $
                      intercalate ":" $
                        splitDirectories $
                          dropExtensions file
               in case fa of
                    Refresh _ ls ->
                      let dir = snd $ minimumOn1 fst (fst <$> ls)
                       in ins tplType name <$> readFileBS (dir </> file)
                    Delete -> pure \s -> s {expansions = deleteExpansion name (expansions s)}
   in unionMount sources patterns exclude initial handler
