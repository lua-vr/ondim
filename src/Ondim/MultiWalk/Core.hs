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
import Type.Reflection (SomeTypeRep (..), TypeRep, eqTypeRep, someTypeRep, typeRep, type (:~~:) (HRefl))
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
  Either ExpansionFailure (Expansion m a, DefinitionSite)
fromSomeExpansion (TextData site t)
  | Just f <- fromText = Right (const $ f <$> t, site)
  | otherwise = Left ExpansionNoFromText
fromSomeExpansion (GlobalExpansion site e) = Right (e, site)
fromSomeExpansion (SomeExpansion t site v)
  | Just HRefl <- t `eqTypeRep` typeRep @a = Right (v, site)
  | otherwise = Left $ ExpansionWrongType (SomeTypeRep t)
fromSomeExpansion (NamespaceData (Namespace n))
  | Just v <- Map.lookup "" n = fromSomeExpansion v
fromSomeExpansion NamespaceData {} =
  Left $ ExpansionWrongType (someTypeRep (Proxy @Namespace))

splitExpansionKey :: Text -> [Text]
splitExpansionKey = T.split (\c -> c /= '-' && not (isLetter c))

lookupExpansion :: Text -> Namespace m -> Maybe (SomeExpansion m)
lookupExpansion (splitExpansionKey -> keys) (Namespace e) = go keys e
  where
    go [] _ = Nothing
    go [k] m = Map.lookup k m
    go (k : ks) m = case Map.lookup k m of
      Just (NamespaceData (Namespace n)) -> go ks n
      _ -> Nothing

insertExpansion :: Text -> SomeExpansion m -> Namespace m -> Namespace m
insertExpansion (splitExpansionKey -> keys) e (Namespace es) = Namespace $ go keys es
  where
    go [] = id
    go [k] = Map.insert k e
    go (k : ks) =
      flip Map.alter k $
        Just . NamespaceData . Namespace . \case
          Just (NamespaceData (Namespace n)) -> go ks n
          _ -> go ks mempty

deleteExpansion :: Text -> Namespace m -> Namespace m
deleteExpansion (splitExpansionKey -> keys) (Namespace es) = Namespace $ go keys es
  where
    go [] = id
    go [k] = Map.delete k
    go (k : ks) = flip Map.alter k \case
      Just (NamespaceData (Namespace n)) ->
        Just $ NamespaceData $ Namespace $ go ks n
      _ -> Nothing

-- * Lifiting

getTextData :: Monad m => Text -> Ondim m (Either ExpansionFailure Text)
getTextData name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  case mbValue of
    Just (TextData _ text) -> Right <$> text
    Just _ -> return $ Left (ExpansionFailureOther "Identifier not bound to text data.")
    Nothing -> return $ Left ExpansionNotBound

getNamespace ::
  Monad m =>
  Text ->
  Ondim m (Either ExpansionFailure (Namespace m))
getNamespace name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  case mbValue of
    Just (NamespaceData n) -> return $ Right n
    Just _ -> return $ Left (ExpansionFailureOther "Identifier not bound to a namespace.")
    Nothing -> return $ Left ExpansionNotBound

getExpansion ::
  forall t m.
  GlobalConstraints m t =>
  Text ->
  Ondim m (Either ExpansionFailure (Expansion m t))
getExpansion name = do
  mbValue <- Ondim $ gets $ lookupExpansion name . expansions
  return do
    value <- maybeToRight ExpansionNotBound mbValue
    (expansion, site) <- fromSomeExpansion value
    return $ expCtx name site . expansion
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
        Right expansion -> expansion node
        Left e -> throwExpFailure @t name e
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
  forall ls t m.
  ( Monad m,
    HasSub GSubTag ls t,
    AllMods CanLift ls
  ) =>
  t ->
  Ondim m t
modSubLift = HS.modSub @OCTag @GSubTag @ls @t (Proxy @CanLift) (\(_ :: Proxy s) -> liftSub @s)
{-# INLINEABLE modSubLift #-}

expCtx :: forall m a. Monad m => Text -> DefinitionSite -> Ondim m a -> Ondim m a
expCtx name site (Ondim ctx) = do
  gst <- Ondim ask
  if depth gst >= 200
    then -- To avoid recursive expansions
      throwException MaxExpansionDepthExceeded
    else
      Ondim $
        local
          ( \s ->
              s
                { depth = depth s + 1,
                  expansionTrace =
                    (name, site)
                      : expansionTrace s
                }
          )
          ctx
