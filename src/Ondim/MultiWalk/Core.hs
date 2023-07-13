{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Core where

import Control.MultiWalk.HasSub (AllMods, GSubTag)
import Control.MultiWalk.HasSub qualified as HS
import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as HMap
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import {-# SOURCE #-} Ondim.MultiWalk.Combinators ()
import Ondim.MultiWalk.State
import Type.Reflection (SomeTypeRep (..), eqTypeRep, someTypeRep, typeRep, (:~~:) (..))
import Prelude hiding (All)

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
  forall b a m.
  (Monad m, OndimNode a, OndimNode b) =>
  DefinitionSite ->
  b ->
  Either OndimFailure (Ondim m [a])
fromTemplate site value
  | Just HRefl <- brep `eqTypeRep` typeRep @a = Right (one <$> lifted)
  | Just cast <- ondimCast = Right $ cast <$> lifted
  | otherwise = Left $ TemplateWrongType (SomeTypeRep brep)
  where
    lifted = withSite site (liftSubstructures value)
    brep = typeRep @b

templateToExpansion ::
  forall m t.
  GlobalConstraints m t =>
  Ondim m [t] ->
  Expansion m t
templateToExpansion tpl inner = do
  callSite <- getCurrentSite
  attrs <- attributes inner
  tpl `binding` do
    "caller" #. do
      "children" #: templateData' callSite (children inner)
      unless (null attrs) $
        "attrs" #. forM_ attrs (uncurry (#@))

fromSomeExpansion ::
  forall a m.
  GlobalConstraints m a =>
  DefinitionSite ->
  SomeExpansion m ->
  Either OndimFailure (Expansion m a, DefinitionSite)
fromSomeExpansion callSite someExp =
  case someExp' of
    (GlobalExpansion site e) -> Right (e, site)
    (SomeExpansion t site v)
      | Just HRefl <- t `eqTypeRep` typeRep @a -> Right (v, site)
      | otherwise -> Left $ ExpansionWrongType (SomeTypeRep t)
    (Template site v) -> do
      thing <- fromTemplate site v
      return (templateToExpansion thing, site)
    NamespaceData {} -> Left $ ExpansionWrongType (someTypeRep (Proxy @Namespace))
  where
    someExp' = case someExp of
      (NamespaceData ns@(Namespace n))
        | Just v <- HMap.lookup "" n -> v
        | FileDefinition _ ext <- callSite,
          Just v <- lookupExpansion ext ns -> v
      _nonNamespace -> someExp

getTemplate :: forall m a. GlobalConstraints m a => Text -> Ondim m (Either OndimFailure [a])
getTemplate name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  case mbValue of
    Just (Template site thing) ->
      bimapM return id $ fromTemplate site thing
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
  site <- getCurrentSite
  return do
    value <- maybeToRight NotBound mbValue
    (expansion, expSite) <- fromSomeExpansion site value
    return $ expCtx name expSite . expansion
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
