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
  forall m n tplTypes.
  (Ord tplTypes) =>
  (MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  -- | Patterns to look for.
  [(tplTypes, FilePattern)] ->
  -- | Insertion
  (tplTypes -> FilePath -> ByteString -> SomeExpansion n) ->
  -- | Places to look for templates, in descending order of priority.
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic' patts ins places =
  let sources = fromList (zip (zip [1 ..] places) places)
      patterns = patts
      exclude = []
      initial = mempty
      handler :: Change (Int, FilePath) tplTypes -> m (OndimState n -> OndimState n)
      handler chg =
        appEndo . mconcat . coerce . join
          <$> forM (toPairs chg) \(tplType, chg') ->
            forM (toPairs chg') \(file, fa) ->
              let name = toText $ intercalate "." $ splitDirectories $ dropExtensions file
               in case fa of
                    Refresh _ ls ->
                      let dir = snd $ minimumOn1 fst (fst <$> ls)
                          fp = dir </> file
                       in readFileBS fp <&> \b s -> s {expansions = insertExpansion name (ins tplType fp b) (expansions s)}
                    Delete -> pure \s -> s {expansions = deleteExpansion name (expansions s)}
   in unionMount sources patterns exclude initial handler
