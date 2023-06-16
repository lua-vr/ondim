{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Core where

import Control.MultiWalk.HasSub (AllMods, GSubTag)
import Control.MultiWalk.HasSub qualified as HS
import Data.Char (isLetter)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import Type.Reflection (TypeRep, eqTypeRep, typeRep, type (:~~:) (HRefl))
import Prelude hiding (All)

-- * CanLift class

class CanLift (s :: Type) where
  liftSub ::
    Monad m =>
    Carrier s ->
    Ondim m (Carrier s)

instance {-# OVERLAPPABLE #-} (Carrier a ~ [a], OndimNode a) => CanLift a where
  liftSub = liftNodes

-- * Expansion state manipulation

getSomeFilter :: forall a m. GlobalConstraints m a => SomeFilter m -> Maybe (Filter m a)
getSomeFilter x
  | SomeFilter t v <- x,
    Just HRefl <- t `eqTypeRep` rep =
      Just v
  | otherwise = Nothing
  where
    rep = typeRep :: TypeRep a

getSomeMapFilter :: forall a m. GlobalConstraints m a => SomeFilter m -> Maybe (MapFilter m a)
getSomeMapFilter x
  | SomeMapFilter t v <- x,
    Just HRefl <- t `eqTypeRep` rep =
      Just v
  | otherwise = Nothing
  where
    rep = typeRep :: TypeRep a

fromSomeExpansion ::
  forall a m.
  GlobalConstraints m a =>
  SomeExpansion m ->
  Maybe (Expansion m a)
fromSomeExpansion (TextData _ t)
  | Just f <- fromText = Just (const $ f <$> t)
  | otherwise = Nothing
fromSomeExpansion (GlobalExpansion _ e) = Just e
fromSomeExpansion (SomeExpansion t _ v)
  | Just HRefl <- t `eqTypeRep` typeRep @a = Just v
  | otherwise = Nothing
fromSomeExpansion Namespace {} = Nothing

splitExpansionKey :: Text -> [Text]
splitExpansionKey = T.split (\c -> c /= '-' && not (isLetter c))

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

-- * Lifiting

getTextData :: Monad m => Text -> Ondim m (Maybe Text)
getTextData name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  case mbValue of
    Just (TextData _ text) -> Just <$> text
    _ -> return Nothing

getNamespace :: Monad m => Text -> Ondim m (Maybe (Expansions m))
getNamespace name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  case mbValue of
    Just (Namespace n) -> return $ Just n
    _ -> return Nothing

getExpansion ::
  forall t m.
  GlobalConstraints m t =>
  Text ->
  Ondim m (Maybe (Expansion m t))
getExpansion name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  return $ (expCtx name .) <$> (fromSomeExpansion =<< mbValue)
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
  apFilters <-
    Ondim $
      gets $
        foldr (\f g -> f node . g) id
          . mapMaybe (getSomeFilter @t)
          . toList
          . filters
  apFilters $
    case identify node of
      Just name -> expand name
      _ -> one <$> liftSubstructures node
  where
    expand name =
      getExpansion name >>= \case
        Just expansion -> expansion node
        Nothing -> one <$> liftSubstructures node
{-# INLINEABLE liftNode #-}

-- | Lift a list of nodes, applying filters.
liftNodes ::
  forall m t.
  (Monad m, OndimNode t) =>
  [t] ->
  Ondim m [t]
liftNodes nodes = do
  apFilters <-
    Ondim $
      gets $
        foldr (.) id
          . mapMaybe (getSomeMapFilter @t)
          . toList
          . filters
  apFilters $ foldMapM liftNode nodes

-- | Lift only the substructures of a node.
liftSubstructures :: forall m t. (Monad m, OndimNode t) => t -> Ondim m t
liftSubstructures = modSubLift @(ExpTypes t)
{-# INLINEABLE liftSubstructures #-}

-- * Lifting functions

modSubLift ::
  forall ls m t.
  ( Monad m,
    HasSub GSubTag ls t,
    AllMods CanLift ls
  ) =>
  t ->
  Ondim m t
modSubLift = HS.modSub @OCTag @GSubTag @ls @t (Proxy @CanLift) (\(_ :: Proxy s) -> liftSub @s)
{-# INLINEABLE modSubLift #-}

expCtx :: forall m a. Monad m => Text -> Ondim m a -> Ondim m a
expCtx name (Ondim ctx) = do
  gst <- Ondim ask
  if depth gst >= 200
    then -- To avoid recursive expansions
      throwOndim MaxExpansionDepthExceeded
    else Ondim $ local (\s -> s {depth = depth s + 1, expansionTrace = name : expansionTrace s}) ctx
