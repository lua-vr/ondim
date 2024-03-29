{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Ondim.MultiWalk.Core
  ( expandNode,
    expandNodes,
    expandSubstructures,
    expandSpecList,
    getExpansion,
    getTemplate,
    getNamespace,
    getText,
  ) where

import Control.MultiWalk.HasSub (AllMods, GSubTag)
import Control.MultiWalk.HasSub qualified as HS
import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as HMap
import Data.Typeable (eqT, (:~:) (..))
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import Ondim.State
import Type.Reflection (SomeTypeRep, someTypeRep)
import Prelude hiding (All)

-- Get stuff from state

fromTemplate ::
  forall b a s.
  (OndimNode a, OndimNode b) =>
  DefinitionSite ->
  b ->
  Either OndimFailure (Ondim s [a])
fromTemplate site value
  | Just Refl <- eqT @a @b = Right (one <$> lifted)
  | Just cast <- ondimCast = Right $ cast <$> lifted
  | otherwise = Left $ TemplateWrongType (someTypeRep (Proxy @b))
  where
    lifted = withSite site (expandSubstructures value)

templateToExpansion ::
  forall s t.
  (OndimNode t) =>
  Ondim s [t] ->
  Expansion s t
templateToExpansion tpl inner = do
  callSite <- getCurrentSite
  attrs <- attributes inner
  tpl `binding` do
    "caller" #. do
      "children" #: TemplateData callSite (children inner)
      unless (null attrs) $
        "attrs"
          #. forM_ attrs (uncurry (#@))

fromSomeExpansion ::
  forall a s.
  (OndimNode a) =>
  DefinitionSite ->
  NamespaceItem s ->
  Either OndimFailure (Expansion s a, DefinitionSite)
fromSomeExpansion callSite someExp =
  case someExp' of
    (PolyExpansion site e) -> Right (e, site)
    (TypedExpansion @t site v)
      | Just Refl <- eqT @a @t -> Right (v, site)
      | otherwise -> Left $ ExpansionWrongType (someTypeRep (Proxy @t))
    (TemplateData site v) -> do
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
          Just v <- lookup ext ns ->
            v
      _nonNamespace -> someExp

getText :: Text -> Ondim s (Either OndimFailure Text)
getText name = do
  mbValue <- lookup name . expansions <$> getOndimS
  case mbValue of
    Just (TemplateData @t site thing)
      | Just Refl <- eqT' @Text @t -> return $ Right thing
      | Just cast <- nodeAsText -> Right . cast <$> withSite site (expandSubstructures thing)
      | otherwise -> return $ Left $ TemplateWrongType (typeRep' @t)
    -- bimapM return id $ fromTemplate site thing
    Just _ -> return $ Left (FailureOther "Identifier not bound to a template.")
    Nothing -> return $ Left NotBound
  where
    -- Small hacks (this is due to hs-boot superclass witness issues)
    eqT' :: forall a b. (Typeable a, OndimNode b) => Maybe (a :~: b)
    eqT' = eqT @a @b
    typeRep' :: forall a. (OndimNode a) => SomeTypeRep
    typeRep' = someTypeRep (Proxy @a)

getTemplate :: (OndimNode a) => Text -> Ondim s (Either OndimFailure [a])
getTemplate name = do
  mbValue <- lookup name . expansions <$> getOndimS
  case mbValue of
    Just (TemplateData site thing) ->
      bimapM return id $ fromTemplate site thing
    Just _ -> return $ Left (FailureOther "Identifier not bound to a template.")
    Nothing -> return $ Left NotBound

getNamespace :: Text -> Ondim s (Either OndimFailure (Namespace s))
getNamespace name = do
  mbValue <- lookup name . expansions <$> getOndimS
  case mbValue of
    Just (NamespaceData n) -> return $ Right n
    Just _ -> return $ Left (FailureOther "Identifier not bound to a namespace.")
    Nothing -> return $ Left NotBound

getExpansion :: (OndimNode t) => Text -> Ondim s (Either OndimFailure (Expansion s t))
getExpansion name = do
  mbValue <- lookup name . expansions <$> getOndimS
  site <- getCurrentSite
  return do
    value <- maybeToRight NotBound mbValue
    (expansion, expSite) <- fromSomeExpansion site value
    return $ expCtx name expSite . expansion
{-# INLINEABLE getExpansion #-}

expCtx :: Text -> DefinitionSite -> Ondim s a -> Ondim s a
expCtx name site (Ondim ctx) = do
  gst <- Ondim $ asks fst
  if depth gst >= 200
    then -- To avoid recursive expansions
      throwException MaxExpansionDepthExceeded
    else
      Ondim $
        local
          ( first \s ->
              s
                { depth = depth s + 1,
                  expansionTrace =
                    (name, site)
                      : expansionTrace s
                }
          )
          ctx

-- * Lifiting

{- | This function recursively expands the node and its substructures according to
   the expansions that are bound in the context.

  More precisely, if the node name matches the name of a bound expansion, then
  it feeds the node directly into the expansion. Otherwise, it runs
  'expandSubstructures' on the node, which essentially amounts to running
  'expandNode' on each substructure.
-}
expandNode :: forall t s. (OndimNode t) => t -> Ondim s [t]
expandNode node = do
  inhibit <- Ondim $ asks (inhibitErrors . fst)
  case identify node of
    Just name ->
      getExpansion name >>= \case
        Right expansion -> expansion node
        Left e
          | inhibit -> continue
          | otherwise -> throwExpFailure @t name e
    Nothing -> continue
  where
    continue = one <$> expandSubstructures node
{-# INLINEABLE expandNode #-}

-- | Expand a list of nodes and concatenate results.
expandNodes ::
  forall t s.
  (OndimNode t) =>
  [t] ->
  Ondim s [t]
expandNodes = expandSpec @(NodeListSpec t)
{-# INLINEABLE expandNodes #-}

-- | Expand only the substructures of a node.
expandSubstructures :: forall t s. (OndimNode t) => t -> Ondim s t
expandSubstructures = expandSpecList @(ExpTypes t)
{-# INLINEABLE expandSubstructures #-}

-- * Lifting functions

-- | Expand a list of nodes according to a spec list.
expandSpecList ::
  forall ls t s.
  ( HasSub GSubTag ls t,
    AllMods Expansible ls
  ) =>
  t ->
  Ondim s t
expandSpecList = HS.modSub @OCTag @GSubTag @ls @t (Proxy @Expansible) (\(_ :: Proxy r) -> expandSpec @r)
{-# INLINEABLE expandSpecList #-}
