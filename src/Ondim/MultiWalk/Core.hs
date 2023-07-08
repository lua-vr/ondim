{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Core where

import Control.MultiWalk.HasSub (AllMods, GSubTag)
import Control.MultiWalk.HasSub qualified as HS
import Data.HashMap.Strict qualified as HMap
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import Ondim.MultiWalk.State
import Type.Reflection (SomeTypeRep (..), eqTypeRep, someTypeRep, typeRep, (:~~:) (..))
import Prelude hiding (All)
import Data.Bitraversable (bimapM)

-- * CanLift class

class CanLift (s :: Type) where
  liftSub ::
    Monad m =>
    Carrier s ->
    Ondim m (Carrier s)

instance {-# OVERLAPPABLE #-} (Carrier a ~ [a], OndimNode a) => CanLift a where
  liftSub = liftNodes

-- Get stuff from state

fromTemplate ::
  forall b a.
  (OndimNode a, OndimCast b) =>
  b ->
  Either OndimFailure [a]
fromTemplate value
  | Just HRefl <- brep `eqTypeRep` typeRep @a = Right [value]
  | Just cast <- ondimCast = Right $ cast value
  | otherwise = Left $ TemplateWrongType (SomeTypeRep brep)
  where
    brep = typeRep @b

templateToExpansion ::
  forall m t.
  GlobalConstraints m t =>
  DefinitionSite ->
  [t] ->
  Expansion m t
templateToExpansion site tpl inner = do
  callSite <- getCurrentSite
  attrs <- attributes inner
  withSite site (liftNodes tpl)
    `binding` do
      "caller" #. do
        "children" #: templateData' callSite (children inner)
        unless (null attrs) $
          "attrs" #. forM_ attrs (uncurry (#@))

fromSomeExpansion ::
  forall a m.
  GlobalConstraints m a =>
  SomeExpansion m ->
  Either OndimFailure (Expansion m a, DefinitionSite)
fromSomeExpansion (GlobalExpansion site e) = Right (e, site)
fromSomeExpansion (SomeExpansion t site v)
  | Just HRefl <- t `eqTypeRep` typeRep @a = Right (v, site)
  | otherwise = Left $ ExpansionWrongType (SomeTypeRep t)
fromSomeExpansion (Template site v) = do
  thing <- fromTemplate v
  return (templateToExpansion site thing, site)
fromSomeExpansion (NamespaceData (Namespace n))
  | Just v <- HMap.lookup "" n = fromSomeExpansion v
  | otherwise = Left $ ExpansionWrongType (someTypeRep (Proxy @Namespace))

getTemplate :: forall m a. GlobalConstraints m a => Text -> Ondim m (Either OndimFailure [a])
getTemplate name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  case mbValue of
    Just (Template _ thing) ->
      bimapM return liftNodes $ fromTemplate thing
    Just _ -> return $ Left (FailureOther "Identifier not bound to a template.")
    Nothing -> return $ Left NotBound

getTemplateFold :: (Monoid a, GlobalConstraints m a) => Text -> Ondim m (Either OndimFailure a)
getTemplateFold name = second mconcat <$> getTemplate name

getNamespace ::
  Monad m =>
  Text ->
  Ondim m (Either OndimFailure (Namespace m))
getNamespace name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  case mbValue of
    Just (NamespaceData n) -> return $ Right n
    Just _ -> return $ Left (FailureOther "Identifier not bound to a namespace.")
    Nothing -> return $ Left NotBound

getExpansion ::
  forall t m.
  GlobalConstraints m t =>
  Text ->
  Ondim m (Either OndimFailure (Expansion m t))
getExpansion name = do
  mbValue <- Ondim $ gets $ lookupExpansion name . expansions
  return do
    value <- maybeToRight NotBound mbValue
    (expansion, site) <- fromSomeExpansion value
    return $ expCtx name site . expansion
{-# INLINEABLE getExpansion #-}

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

getSomeFilter :: forall a m. GlobalConstraints m a => SomeFilter m -> Maybe (Filter m a)
getSomeFilter x
  | SomeFilter t v <- x,
    Just HRefl <- t `eqTypeRep` rep =
      Just v
  | otherwise = Nothing
  where
    rep = typeRep @a

getSomeMapFilter :: forall a m. GlobalConstraints m a => SomeFilter m -> Maybe (MapFilter m a)
getSomeMapFilter x
  | SomeMapFilter t v <- x,
    Just HRefl <- t `eqTypeRep` rep =
      Just v
  | otherwise = Nothing
  where
    rep = typeRep @a

-- * Lifiting

{- | This function recursively lifts the nodes into an unvaluated state, that will
   be evaluated with the defined expansions.
-}
liftNode ::
  forall m t.
  GlobalConstraints m t =>
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
  GlobalConstraints m t =>
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
{-# INLINEABLE liftNodes #-}

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
