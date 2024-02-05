{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Ondim.MultiWalk.Core where

import Control.MultiWalk.HasSub (AllMods, GSubTag)
import Control.MultiWalk.HasSub qualified as HS
import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as HMap
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import Ondim.MultiWalk.State
import Type.Reflection (SomeTypeRep (..), eqTypeRep, someTypeRep, typeRep, (:~~:) (..))
import Prelude hiding (All)

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
  (OndimNode t, Monad m) =>
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
  (OndimNode a, Monad m) =>
  DefinitionSite ->
  SomeExpansion m ->
  Either OndimFailure (Expansion m a, DefinitionSite)
fromSomeExpansion callSite someExp =
  case someExp' of
    (GlobalExpansion site e) -> Right (e, site)
    (SomeExpansion t site v)
      | Just HRefl <- t `eqTypeRep` typeRep @a -> Right (v, site)
      | otherwise -> Left $ ExpansionWrongType (SomeTypeRep t)
    (Template _ site v) -> do
      thing <- fromTemplate site v
      return (templateToExpansion thing, site)
    NamespaceData {} -> Left $ ExpansionWrongType (someTypeRep (Proxy @Namespace))
  where
    -- The empty string "" acts as a default expansion for the namespace.
    -- When calling from a file, the file extension also acts as a default.
    someExp' = case someExp of
      (NamespaceData ns@(Namespace n))
        | Just v <- HMap.lookup "" n -> v
        | FileDefinition _ ext <- callSite,
          Just v <- lookupExpansion ext ns ->
            v
      _nonNamespace -> someExp

getText' :: forall m. Monad m => [Text] -> Ondim m (Either OndimFailure Text)
getText' name = do
  mbValue <- Ondim $ gets (lookupExpansion' name . expansions)
  case mbValue of
    Just (Template trep site thing)
      | Just HRefl <- typeRep @Text `eqTypeRep` trep -> return $ Right thing
      | Just cast <- nodeAsText -> Right . cast <$> withSite site (liftSubstructures thing)
      | otherwise -> return $ Left $ TemplateWrongType (SomeTypeRep trep)
    -- bimapM return id $ fromTemplate site thing
    Just _ -> return $ Left (FailureOther "Identifier not bound to a template.")
    Nothing -> return $ Left NotBound

getText :: forall m. Monad m => Text -> Ondim m (Either OndimFailure Text)
getText = getText' . splitExpansionKey

getTemplate' :: forall m a. (OndimNode a, Monad m) => [Text] -> Ondim m (Either OndimFailure [a])
getTemplate' name = do
  mbValue <- Ondim $ gets (lookupExpansion' name . expansions)
  case mbValue of
    Just (Template _ site thing) ->
      bimapM return id $ fromTemplate site thing
    Just _ -> return $ Left (FailureOther "Identifier not bound to a template.")
    Nothing -> return $ Left NotBound

getTemplate :: (OndimNode a, Monad m) => Text -> Ondim m (Either OndimFailure [a])
getTemplate = getTemplate' . splitExpansionKey

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
  (OndimNode t, Monad m) =>
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

-- * Lifiting

{- | This function recursively lifts the nodes into an unvaluated state, that will
   be evaluated with the defined expansions.
-}
liftNode ::
  forall t m.
  (OndimNode t, Monad m) =>
  t ->
  Ondim m [t]
liftNode node = do
  inhibit <- Ondim $ asks inhibitErrors
  case identify node of
    Just name ->
      getExpansion name >>= \case
        Right expansion -> expansion node
        Left e
          | inhibit -> continue
          | otherwise -> throwExpFailure @t name e
    _ -> continue
  where
    continue = one <$> liftSubstructures node
{-# INLINEABLE liftNode #-}

-- | Lift a list of nodes, applying filters.
liftNodes ::
  forall m t.
  (OndimNode t, Monad m) =>
  [t] ->
  Ondim m [t]
liftNodes = liftSub @(NodeListSpec t)
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
