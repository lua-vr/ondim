{- | This module defines helper functions for loading templates from a list of
   directories, optionally using @fsnotify@ for watching and reloading on file
   changes. There is also a helper for loading templates at compile-time via the
   @file-embed@ package.
-}
module Ondim.Loading
  ( loadTemplates,
    loadTemplatesDynamic,
    loadTemplatesEmbed,
    -- * \"Advanced\" usage
    --
    -- | There are default 'LoadConfig's inside each target's respective
    -- modules, but you can also use the definitions below to customize them if
    -- you wish.
    LoadConfig (..),
    LoadFn,
    loadFnSimple,
    TemplateLoadingException (..)
  ) where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Data.Map ((!))
import Ondim
import Ondim.Debug
import Relude.Extra (minimumOn1, toPairs)
import System.FilePath (splitDirectories, (</>))
import System.FilePattern (FilePattern, matchMany)
import System.UnionMount

-- | Some template loading (impure) exception.
newtype TemplateLoadingException = TemplateLoadingException String
  deriving (Eq, Show)
  deriving anyclass (Exception)

-- | A recipe to create templates from file contents.
type LoadFn n =
  -- | Filepath
  FilePath ->
  -- | File contents
  LByteString ->
  -- | Resulting state data
  NamespaceItem n

-- | Default way to load a template. In most cases you should use this recipe.
loadFnSimple :: (OndimNode a) => (FilePath -> LByteString -> Either String a) -> LoadFn n
loadFnSimple fn fp bs = templateData' site $ either throw' id $ fn fp bs
  where
    site = fileSite fp
    throw' = throw . TemplateLoadingException

fpToIdentifier :: FilePath -> Text
fpToIdentifier = toText . intercalate "." . splitDirectories

loadFnToUpdate :: LoadFn n -> FilePath -> Text -> LByteString -> OndimState n -> OndimState n
loadFnToUpdate fn fp name bs s =
  s {expansions = insert name res (expansions s)}
  where
    res = fn fp bs

-- | Configuration for loading templates of a specific type.
data LoadConfig n = LoadConfig
  { patterns :: [FilePattern],
    -- ^ Glob patterns to search for files.
    loadFn :: LoadFn n,
    -- ^ Recipe to load the templates.
    initialState :: OndimState n
    -- ^ Initial state. You can use this to set some default expansions or
    -- templates that may be overshadowed by file templates.
  }

{- | Load templates from a list of directories in descending order of priority,
   and return the inital state and a watcher action that takes a handler to
   update the state when templates get updated on disk.
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
                    Delete -> pure \s -> s {expansions = delete name (expansions s)} <> initial
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
