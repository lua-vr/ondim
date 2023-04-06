{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.MultiWalk.Core
  ( Ondim (..),
    OndimNode (..),
    liftNode,
    liftNodes,
    liftSubstructures,
    OndimState (..),
    initialGS,
    OndimException (..),
    throwNotBound,
    throwCustom,
    GlobalConstraints,
    Expansion,
    GlobalExpansion,
    SomeExpansion (..),
    toSomeExpansion,
    splitExpansionKey,
    lookupExpansion,
    insertExpansion,
    deleteExpansion,
    Expansions (..),
    getExpansion,
    getTextData,
    Filter,
    GlobalFilter,
    SomeFilter (..),
    toSomeFilter,
    Filters,
    CanLift (..),
    modSubLift,
    Substructure (..),
    getSubstructure,
    getSubstructure',
    modSubstructureM,
    modSubstructureM',
    Attribute,
    Under,
    MatchWith,
    Conversible (..),
    Converting,
    OneSub,
    HasSub,
    ToSpec,
    ToSpecSel,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Ondim.MultiWalk.Basic
import Type.Reflection (TypeRep, eqTypeRep, typeRep, type (:~~:) (HRefl))
import Prelude hiding (All)

-- * State data

toSomeFilter :: Typeable a => Filter m a -> SomeFilter m
toSomeFilter = SomeFilter typeRep

getSomeFilter :: forall a m. GlobalConstraints m a => SomeFilter m -> Maybe (Filter m a)
getSomeFilter (GlobalFilter v) = Just v
getSomeFilter (SomeFilter t v)
  | Just HRefl <- t `eqTypeRep` rep = Just v
  | otherwise = Nothing
  where
    rep = typeRep :: TypeRep a

toSomeExpansion :: Typeable a => Expansion m a -> SomeExpansion m
toSomeExpansion = SomeExpansion typeRep

getSomeExpansion ::
  forall a m.
  GlobalConstraints m a =>
  SomeExpansion m ->
  Maybe (Expansion m a)
getSomeExpansion (TextData t)
  | Just f <- fromText = Just (const $ pure $ f t)
  | otherwise = Nothing
getSomeExpansion (GlobalExpansion e) = Just e
getSomeExpansion (SomeExpansion t v)
  | Just HRefl <- t `eqTypeRep` typeRep @a = Just v
  | otherwise = Nothing
getSomeExpansion Namespace {} = Nothing

splitExpansionKey :: Text -> [Text]
splitExpansionKey = T.split (\c -> c == '.' || c == ':')

lookupExpansion :: Text -> Expansions m -> Maybe (SomeExpansion m)
lookupExpansion (splitExpansionKey -> keys) (Expansions e) = go keys e
  where
    go [] _ = Nothing
    go [k] m = Map.lookup k m
    go (k : ks) m = case Map.lookup k m of
      Just (Namespace (Expansions n)) -> go ks n
      _ -> Nothing

insertExpansion :: Text -> SomeExpansion m -> Expansions m -> Expansions m
insertExpansion (splitExpansionKey -> keys) e (Expansions es) = Expansions $ go keys es
  where
    go [] = id
    go [k] = Map.insert k e
    go (k : ks) =
      flip Map.alter k $
        Just . Namespace . Expansions . \case
          Just (Namespace (Expansions n)) -> go ks n
          _ -> go ks mempty

deleteExpansion :: Text -> Expansions m -> Expansions m
deleteExpansion (splitExpansionKey -> keys) (Expansions es) = Expansions $ go keys es
  where
    go [] = id
    go [k] = Map.delete k
    go (k : ks) = flip Map.alter k \case
      Just (Namespace (Expansions n)) -> Just $ Namespace $ Expansions $ go ks n
      _ -> Nothing

initialGS :: OndimGS
initialGS = OndimGS 0 []

-- * Exceptions

throwNotBound ::
  Monad m =>
  Text ->
  Ondim m s
throwNotBound name =
  throwError . ExpansionNotBound name
    =<< Ondim (asks expansionTrace)

throwCustom ::
  Monad m =>
  Text ->
  Ondim m s
throwCustom name =
  throwError . CustomException name
    =<< Ondim (asks expansionTrace)

-- * Lifiting

getTextData :: Monad m => Text -> Ondim m (Maybe Text)
getTextData name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  return do
    TextData text <- mbValue
    return text

getExpansion ::
  forall t m.
  GlobalConstraints m t =>
  Text ->
  Ondim m (Maybe (Expansion m t))
getExpansion name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  return do
    expansion <- getSomeExpansion =<< mbValue
    Just (expCtx name . expansion)
{-# INLINEABLE getExpansion #-}

{- | This function recursively lifts the nodes into an unvaluated state, that will
   be evaluated with the defined expansions.
-}
liftNode ::
  forall m t.
  (Monad m, OndimNode t) =>
  t ->
  Ondim m [t]
liftNode node = do
  apFilters <- Ondim $ gets $ foldr (\f g -> f node . g) id . mapMaybe (getSomeFilter @t) . toList . filters
  apFilters $
    case identify node of
      Just name -> expand name
      _ -> one <$> liftSubstructures node
  where
    expand name =
      getExpansion name >>= \case
        Just expansion -> expansion node
        Nothing -> withDebugCtx id (name :) $ one <$> liftSubstructures node
{-# INLINEABLE liftNode #-}

-- | Lift a list of nodes, applying filters.
liftNodes ::
  forall m t.
  (Monad m, OndimNode t) =>
  [t] ->
  Ondim m [t]
liftNodes = foldMapM liftNode

-- | Lift only the substructures of a node.
liftSubstructures :: forall m t. (Monad m, OndimNode t) => t -> Ondim m t
liftSubstructures = modSubLift @(ExpTypes t)
{-# INLINEABLE liftSubstructures #-}

instance {-# OVERLAPPABLE #-} (Carrier a ~ [a], OndimNode a) => CanLift a where
  liftSub = liftNodes

-- * Expansion context

withDebugCtx ::
  forall m a.
  Monad m =>
  (Int -> Int) ->
  ([Text] -> [Text]) ->
  Ondim m a ->
  Ondim m a
withDebugCtx f g =
  Ondim
    . local
      ( \gs ->
          gs
            { expansionDepth = f (expansionDepth gs),
              expansionTrace = g (expansionTrace gs)
            }
      )
    . unOndimT

expCtx :: forall m a. Monad m => Text -> Ondim m a -> Ondim m a
expCtx name ctx = do
  gst <- Ondim ask
  if expansionDepth gst >= 200
    then -- To avoid recursive expansions
      throwError (MaxExpansionDepthExceeded $ expansionTrace gst)
    else withDebugCtx (+ 1) (name :) ctx

-- * Attributes

instance OndimNode Text where
  type ExpTypes Text = '[]

instance OndimNode Attribute where
  type ExpTypes Attribute = '[ToSpec (OneSub Text)]
  identify = Just . fst
