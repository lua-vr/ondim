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
    withFilter,
    withFilters,
    withoutFilters,

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
    -- Filter map
    FilterMap,
    mapToFilters,
    bindingFilters,
    -- Filter constructors
    ($:),
    someFilter,
    ($#),
    mapFilter,
    ($*),

    -- * Altering namespaces
    splitExpansionKey,
    lookupExpansion,
    insertExpansion,
    deleteExpansion,
  )
where

import Control.Monad.Writer.CPS
import Data.Char (isLetter)
import Data.HashMap.Strict qualified as HMap
import Data.Map qualified as Map
import Data.Text qualified as T
import Ondim.MultiWalk.Basic
import Type.Reflection (typeRep)
import Ondim.MultiWalk.Class (OndimNode)

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

withFilter ::
  Monad m =>
  Text ->
  Maybe (SomeFilter m) ->
  Ondim m a ->
  Ondim m a
withFilter name ex st = do
  pEx <- Ondim $ gets (Map.lookup name . filters)
  Ondim $ modify' \s -> s {filters = insOrDel ex (filters s)}
  st <* modifyOndimS \s -> s {filters = insOrDel pEx (filters s)}
  where
    insOrDel x = Map.alter (const x) name

-- | "Bind" new namespace locally.
withNamespace :: Monad m => Namespace m -> Ondim m a -> Ondim m a
withNamespace (Namespace exps) o =
  foldr (\(k, v) -> withSomeExpansion k (Just v)) o (HMap.toList exps)

-- | "Bind" filters locally.
withFilters :: Monad m => Filters m -> Ondim m a -> Ondim m a
withFilters filt o = foldr (\(k, v) -> withFilter k (Just v)) o (Map.toList filt)

-- | "Unbind" many expansions locally.
withoutExpansions :: Monad m => [Text] -> Ondim m a -> Ondim m a
withoutExpansions names o = foldr (`withSomeExpansion` Nothing) o names

-- | "Unbind" many expansions locally.
withoutFilters :: Monad m => [Text] -> Ondim m a -> Ondim m a
withoutFilters names o = foldr (`withFilter` Nothing) o names

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
templateData = Template callStackSite

templateData' :: forall a m. (HasCallStack, OndimNode a) => DefinitionSite -> a -> SomeExpansion m
templateData' = Template

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

-- Filters

type FilterMap m = Writer [(Text, Maybe (SomeFilter m))] ()

infixr 0 $:

($:) :: Text -> SomeFilter m -> FilterMap m
name $: ex = name #<> Just ex

someFilter :: Typeable t => Filter m t -> SomeFilter m
someFilter = SomeFilter typeRep

infixr 0 $#

($#) :: Typeable t => Text -> Filter m t -> FilterMap m
name $# ex = name $: someFilter ex

mapFilter :: Typeable t => MapFilter m t -> SomeFilter m
mapFilter = SomeMapFilter typeRep

infixr 0 $*

($*) :: Typeable t => Text -> MapFilter m t -> FilterMap m
name $* ex = name $: mapFilter ex

mapToFilters :: FilterMap m -> Filters m
mapToFilters ex = foldl' go mempty exps
  where
    go = flip $ uncurry Map.insert
    exps = mapMaybe sequence $ execWriter ex

-- | Infix version of @withExpansions@ to bind using MapSyntax.
binding ::
  Monad m =>
  Ondim m a ->
  ExpansionMap m ->
  Ondim m a
binding o exps =
  let kvs = execWriter exps
   in foldl' (flip $ uncurry withSomeExpansion) o kvs

-- | Infix version of @withFilters@ to bind using MapSyntax.
bindingFilters ::
  Monad m =>
  Ondim m a ->
  FilterMap m ->
  Ondim m a
bindingFilters o filts =
  let kvs = execWriter filts
   in foldl' (flip $ uncurry withFilter) o kvs

splitExpansionKey :: Text -> [Text]
splitExpansionKey = T.split (\c -> c /= '-' && not (isLetter c))

lookupExpansion :: Text -> Namespace m -> Maybe (SomeExpansion m)
lookupExpansion (splitExpansionKey -> keys) (Namespace e) = go keys e
  where
    go [] _ = Nothing
    go [k] m = HMap.lookup k m
    go (k : ks) m = case HMap.lookup k m of
      Just (NamespaceData (Namespace n)) -> go ks n
      _ -> Nothing

insertExpansion :: Text -> SomeExpansion m -> Namespace m -> Namespace m
insertExpansion (splitExpansionKey -> keys) e (Namespace es) = Namespace $ go keys es
  where
    go [] = id
    go [k] = HMap.insert k e
    go (k : ks) =
      flip HMap.alter k $
        Just . NamespaceData . Namespace . \case
          Just (NamespaceData (Namespace n)) -> go ks n
          _ -> go ks mempty

deleteExpansion :: Text -> Namespace m -> Namespace m
deleteExpansion (splitExpansionKey -> keys) (Namespace es) = Namespace $ go keys es
  where
    go [] = id
    go [k] = HMap.delete k
    go (k : ks) = flip HMap.alter k \case
      Just (NamespaceData (Namespace n)) ->
        Just $ NamespaceData $ Namespace $ go ks n
      _ -> Nothing
