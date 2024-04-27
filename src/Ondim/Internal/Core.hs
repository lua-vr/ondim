{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Internal.Core
  ( expandNode,
    expandSubs,
    getExpansion,
    getTemplate,
    getNamespace,
    getText,

    -- Exceptions
    withoutNBErrors,
    withNBErrors,
    catchException,
    throwException,
    throwTemplateError,
    catchFailure,
    throwExpFailure,
  ) where

import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as HMap
import Data.Typeable (eqT, (:~:) (..))
import Ondim.Internal.Basic
import Ondim.Internal.Class
import Ondim.State
import Type.Reflection (SomeTypeRep, someTypeRep)
import Prelude hiding (All)
import Control.Monad.Except (MonadError(..))

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
    lifted = withSite site (expandSubs value)

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
      | Just cast <- nodeAsText -> Right . cast <$> withSite site (expandSubs thing)
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

-- * Expansion

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
    continue = one <$> expandSubs node
{-# INLINEABLE expandNode #-}

instance
  {-# OVERLAPPABLE #-}
  ( OndimNode t
  ) =>
  Expansible [t]
  where
  expandSubs = foldMapM expandNode
  {-# INLINEABLE expandSubs #-}

instance
  {-# OVERLAPPABLE #-}
  ( OndimNode t
  ) =>
  Expansible (Seq t)
  where
  expandSubs = foldMapM (fmap fromList . expandNode)
  {-# INLINEABLE expandSubs #-}

-- * Exceptions

{- | Run subcomputation without (most) "not bound" errors. More specifically, if
'Ondim.expandNode' finds a node whose identifier is not bound, it will not
throw an error and instead treat it as if it had no identifier, i.e., it will
ignore it and recurse into the substructures.
-}
withoutNBErrors :: Ondim s a -> Ondim s a
withoutNBErrors = Ondim . local (first f) . unOndimT
  where
    f r = r {inhibitErrors = True}

-- | Run subcomputation with "not bound" errors.
withNBErrors :: Ondim s a -> Ondim s a
withNBErrors = Ondim . local (first f) . unOndimT
  where
    f r = r {inhibitErrors = False}

catchException ::
  Ondim s a ->
  (OndimException -> Ondim s a) ->
  Ondim s a
catchException (Ondim m) f = Ondim $ catchError m (unOndimT . f)

throwException :: ExceptionType -> Ondim s a
throwException e = do
  td <- Ondim (asks fst)
  Ondim $ throwError (OndimException e td)

throwTemplateError :: (HasCallStack) => Text -> Ondim s a
throwTemplateError t = throwException (TemplateError callStack t)

catchFailure ::
  Ondim s a ->
  (OndimFailure -> Text -> SomeTypeRep -> TraceData -> Ondim s a) ->
  Ondim s a
catchFailure (Ondim m) f = Ondim $ catchError m \(OndimException exc tdata) ->
  case exc of
    Failure trep name e -> unOndimT $ f e name trep tdata
    _other -> m

throwExpFailure ::
  forall t s a.
  (Typeable t) =>
  Text ->
  OndimFailure ->
  Ondim s a
throwExpFailure t f =
  throwException $ Failure (someTypeRep (Proxy @t)) t f
