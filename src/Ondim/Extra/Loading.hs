{-# LANGUAGE GADTs #-}

module Ondim.Extra.Loading where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Data.Map ((!))
import Ondim
import Relude.Extra (minimumOn1, toPairs)
import System.FilePath (splitDirectories, (</>))
import System.FilePattern (FilePattern, matchMany)
import System.UnionMount

newtype TemplateLoadingError = TemplateLoadingException String
  deriving (Eq, Show)
  deriving anyclass (Exception)

data LoadFn where
  LoadFn :: OndimNode a => (FilePath -> LByteString -> Either String a) -> LoadFn

fpToIdentifier :: FilePath -> Text
fpToIdentifier = toText . intercalate "." . splitDirectories

loadFnToUpdate :: LoadFn -> FilePath -> Text -> LByteString -> OndimState n -> OndimState n
loadFnToUpdate (LoadFn fn) fp name bs s =
  s {expansions = insertExpansion name res (expansions s)}
  where
    site = fileSite fp
    throw' = throw . TemplateLoadingException
    res = templateData' site $ either throw' id $ fn fp bs

data LoadConfig n = LoadConfig
  { patterns :: [FilePattern],
    loadFn :: LoadFn,
    initialState :: OndimState n
  }

{- | Load templates from a list of directories in descending order of priority,
and return the inital state and a watcher action that takes a handler to update
the state when templates get updated on disk.
-}
loadTemplatesDynamic ::
  forall m n.
  (MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  -- | Loading configurations
  [LoadConfig n] ->
  -- | Places to look for templates, in descending order of priority.
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic cfgs places =
  let sources = fromList (zip (zip [1 ..] places) places)
      cfgMap = fromList $ [(i, f) | (i, loadFn -> f) <- zip [1 ..] cfgs]
      patts = [(i, p) | (i, patterns -> ps) <- zip [1 ..] cfgs, p <- ps]
      exclude = []
      initial = foldMap' initialState cfgs
      handler :: Change (Int, FilePath) Int -> m (OndimState n -> OndimState n)
      handler chg =
        appEndo . mconcat . coerce . join
          <$> forM (toPairs chg) \(i, chg') ->
            forM (toPairs chg') \(file, fa) ->
              let name = fpToIdentifier file
               in case fa of
                    Refresh _ ls ->
                      let dir = snd $ minimumOn1 fst (fst <$> ls)
                          fp = dir </> file
                       in readFileLBS fp <&> loadFnToUpdate (cfgMap ! i) fp name
                    Delete -> pure \s -> s {expansions = deleteExpansion name (expansions s)}
   in unionMount sources patts exclude initial handler

-- | Load templates from a list of directories in descending order of priority.
loadTemplates :: [LoadConfig n] -> [FilePath] -> IO (OndimState n)
loadTemplates cfgs dirs = fst <$> runNoLoggingT (loadTemplatesDynamic cfgs dirs)

{- | Load templates in pure code from a list of filepaths and bytestrings. Meant
to be used with the @file-embed@ package.
-}
loadTemplatesEmbed :: String -> [LoadConfig n] -> [(FilePath, ByteString)] -> OndimState n
loadTemplatesEmbed prefix cfgs files = foldr go (foldMap' initialState cfgs) res
  where
    patts = [(loadFn c, p) | c <- cfgs, p <- patterns c]
    fdata = [((toLazy bs, fp), fp) | (fp, bs) <- files]
    res = matchMany patts fdata
    go (fn, (bs, fp), _) = loadFnToUpdate fn (prefix </> fp) (fpToIdentifier fp) bs
