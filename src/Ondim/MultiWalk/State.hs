{-# LANGUAGE RankNTypes #-}

module Ondim.MultiWalk.State
  ( -- Manipulate the whole state
    getOndimS,
    modifyOndimS,
    putOndimS,
    -- Modify parts of the state
    withSomeExpansion,
    putSomeExpansion,
    withoutExpansions,
    withNamespace,

    -- * Expansion and Filter maps
    (#<>),
    unbind,
    -- Expansion map
    ExpansionMap,
    mapToNamespace,
    binding,
    -- Expansion constructors
    (#:),
    someExpansion,
    someExpansion',
    (##),
    templateData,
    templateData',
    (#%),
    textData,
    textData',
    (#@),
    globalExpansion,
    globalExpansion',
    (#*),
    namespace,
    namespace',
    (#.),

    -- * Altering namespaces
    splitExpansionKey,
    lookupExpansion,
    insertExpansion,
    deleteExpansion,
    lookupExpansion',
    insertExpansion',
    deleteExpansion',
  )
where

import Control.Monad.Writer.CPS
import Data.Char (isLetter)
import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as T
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class (OndimNode)
import Type.Reflection (typeRep)

-- * User API

-- State manipulation

getOndimS :: Monad m => Ondim m (OndimState m)
getOndimS = Ondim get

modifyOndimS :: Monad m => (OndimState m -> OndimState m) -> Ondim m ()
modifyOndimS = Ondim . modify'

putOndimS :: Monad m => OndimState m -> Ondim m ()
putOndimS = Ondim . put

withSomeExpansion ::
  Monad m =>
  Text ->
  Maybe (SomeExpansion m) ->
  Ondim m a ->
  Ondim m a
withSomeExpansion name ex st = do
  pEx <- Ondim $ gets (lookupExpansion name . expansions)
  Ondim $ modify' \s -> s {expansions = insOrDel ex (expansions s)}
  st <* modifyOndimS \s -> s {expansions = insOrDel pEx (expansions s)}
  where
    insOrDel = maybe (deleteExpansion name) (insertExpansion name)

-- | "Bind" new namespace locally.
withNamespace :: Monad m => Namespace m -> Ondim m a -> Ondim m a
withNamespace (Namespace exps) o =
  foldr (\(k, v) -> withSomeExpansion k (Just v)) o (HMap.toList exps)

-- | "Unbind" many expansions locally.
withoutExpansions :: Monad m => [Text] -> Ondim m a -> Ondim m a
withoutExpansions names o = foldr (`withSomeExpansion` Nothing) o names

-- | Put a new expansion into the local state, modifying the scope.
putSomeExpansion :: Monad m => Text -> SomeExpansion m -> Ondim m ()
putSomeExpansion key ex =
  modifyOndimS \s -> s {expansions = insertExpansion key ex (expansions s)}

-- Expansion and Filter maps

infixr 0 #<>

(#<>) :: Text -> m -> Writer [(Text, m)] ()
name #<> ex = tell [(name, ex)]

unbind :: Text -> Writer [(Text, Maybe m)] ()
unbind k = k #<> Nothing

-- Expansions

type ExpansionMap m = Writer [(Text, Maybe (SomeExpansion m))] ()

infixr 0 #:

(#:) :: Text -> SomeExpansion m -> ExpansionMap m
name #: ex = name #<> Just ex

someExpansion :: (HasCallStack, Typeable t) => Expansion m t -> SomeExpansion m
someExpansion = SomeExpansion typeRep callStackSite

someExpansion' :: Typeable t => DefinitionSite -> Expansion m t -> SomeExpansion m
someExpansion' = SomeExpansion typeRep

infixr 0 ##

(##) :: (HasCallStack, Typeable t) => Text -> Expansion m t -> ExpansionMap m
name ## ex = name #: someExpansion ex

templateData :: forall a m. (HasCallStack, OndimNode a) => a -> SomeExpansion m
templateData = Template typeRep callStackSite

templateData' :: forall a m. (HasCallStack, OndimNode a) => DefinitionSite -> a -> SomeExpansion m
templateData' = Template typeRep

infixr 0 #%

(#%) :: (HasCallStack, OndimNode a) => Text -> a -> ExpansionMap m
name #% ex = name #: templateData ex

textData :: HasCallStack => Text -> SomeExpansion m
textData = templateData

textData' :: DefinitionSite -> Text -> SomeExpansion m
textData' = templateData'

infixr 0 #@

(#@) :: HasCallStack => Text -> Text -> ExpansionMap m
name #@ ex = name #% ex

globalExpansion :: HasCallStack => GlobalExpansion m -> SomeExpansion m
globalExpansion = GlobalExpansion callStackSite

globalExpansion' :: DefinitionSite -> GlobalExpansion m -> SomeExpansion m
globalExpansion' = GlobalExpansion

infixr 0 #*

(#*) :: HasCallStack => Text -> GlobalExpansion m -> ExpansionMap m
name #* ex = name #: globalExpansion ex

mapToNamespace :: ExpansionMap m -> Namespace m
mapToNamespace ex = foldl' go mempty exps
  where
    go = flip $ uncurry insertExpansion
    exps = mapMaybe sequence $ execWriter ex

namespace :: ExpansionMap m -> SomeExpansion m
namespace = NamespaceData . mapToNamespace

namespace' :: Namespace m -> SomeExpansion m
namespace' = NamespaceData

infixr 0 #.

(#.) :: Text -> ExpansionMap m -> ExpansionMap m
name #. ex = name #: namespace ex

-- | Infix version of @withExpansions@ to bind using MapSyntax.
binding ::
  Monad m =>
  Ondim m a ->
  ExpansionMap m ->
  Ondim m a
binding o exps =
  let kvs = execWriter exps
   in foldl' (flip $ uncurry withSomeExpansion) o kvs

splitExpansionKey :: Text -> [Text]
splitExpansionKey = T.split (\c -> c /= '-' && not (isLetter c))

lookupExpansion' :: [Text] -> Namespace m -> Maybe (SomeExpansion m)
lookupExpansion' keys (Namespace e) = go keys e
  where
    go [] _ = Nothing
    go [k] m = HMap.lookup k m
    go (k : ks) m = case HMap.lookup k m of
      Just (NamespaceData (Namespace n)) -> go ks n
      _ -> Nothing

lookupExpansion :: Text -> Namespace m -> Maybe (SomeExpansion m)
lookupExpansion = lookupExpansion' . splitExpansionKey

insertExpansion' :: [Text] -> SomeExpansion m -> Namespace m -> Namespace m
insertExpansion' keys e (Namespace es) = Namespace $ go keys es
  where
    go [] = id
    go [k] = HMap.insert k e
    go (k : ks) =
      flip HMap.alter k $
        Just . NamespaceData . Namespace . \case
          Just (NamespaceData (Namespace n)) -> go ks n
          _ -> go ks mempty

insertExpansion :: Text -> SomeExpansion m -> Namespace m -> Namespace m
insertExpansion = insertExpansion' . splitExpansionKey

deleteExpansion' :: [Text] -> Namespace m -> Namespace m
deleteExpansion' keys v@(Namespace es) =
  case keys of
    [] -> v
    [k] -> Namespace $ HMap.delete k es
    (k : ks) -> Namespace $ go ks k es
  where
    go ks = HMap.update \case
      (NamespaceData n) ->
        case deleteExpansion' ks n of
          x@(Namespace hmap)
            | HMap.null hmap -> Nothing
            | otherwise -> Just $ NamespaceData x
      x -> Just x

deleteExpansion :: Text -> Namespace m -> Namespace m
deleteExpansion = deleteExpansion' . splitExpansionKey
