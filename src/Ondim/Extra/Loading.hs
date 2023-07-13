{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.Extra.Loading where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Data.Map ((!))
import Ondim
import Relude.Extra (minimumOn1, toPairs)
import System.FilePath (splitDirectories, (</>))
import System.FilePattern (FilePattern)
import System.UnionMount

newtype TemplateLoadingError = TemplateLoadingException String
  deriving (Eq, Show)
  deriving anyclass (Exception)

data LoadFn where
  LoadFn :: OndimNode a => (FilePath -> LByteString -> Either String a) -> LoadFn

data LoadConfig = LoadConfig
  { patterns :: [FilePattern],
    loadFn :: LoadFn
  }

loadTemplatesDynamic ::
  forall m n.
  (MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  [LoadConfig] ->
  -- | Places to look for templates, in descending order of priority.
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic cfgs places =
  let sources = fromList (zip (zip [1 ..] places) places)
      cfgMap = fromList $ [(i, f) | (i, loadFn -> f) <- zip [1 ..] cfgs]
      patts = [(i, p) | (i, patterns -> ps) <- zip [1 ..] cfgs, p <- ps]
      exclude = []
      initial = mempty
      handler :: Change (Int, FilePath) Int -> m (OndimState n -> OndimState n)
      handler chg =
        appEndo . mconcat . coerce . join
          <$> forM (toPairs chg) \(i, chg') ->
            case cfgMap ! i of
              LoadFn load ->
                forM (toPairs chg') \(file, fa) ->
                  let name = toText $ intercalate "." $ splitDirectories file
                   in case fa of
                        Refresh _ ls ->
                          let dir = snd $ minimumOn1 fst (fst <$> ls)
                              fp = dir </> file
                              site = fileSite fp
                              throw' = throw . TemplateLoadingException
                              res = templateData' site . either throw' id . load fp
                           in readFileLBS fp <&> \b s ->
                                s {expansions = insertExpansion name (res b) (expansions s)}
                        Delete -> pure \s -> s {expansions = deleteExpansion name (expansions s)}
   in unionMount sources patts exclude initial handler

loadTemplates :: Monad n => [LoadConfig] -> [FilePath] -> IO (OndimState n)
loadTemplates cfgs dirs = fst <$> runNoLoggingT (loadTemplatesDynamic cfgs dirs)
