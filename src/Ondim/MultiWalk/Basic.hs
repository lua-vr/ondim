{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Basic where

import Control.Monad.Except (MonadError (..))
import Control.MultiWalk.HasSub qualified as HS
import Data.HashMap.Strict qualified as Map
import GHC.Exception (SrcLoc)
import GHC.Exts qualified as GHC
import {-# SOURCE #-} Ondim.MultiWalk.Class
import Type.Reflection (SomeTypeRep, TypeRep, someTypeRep)

-- * Monad

newtype Ondim m a = Ondim
  { unOndimT :: ReaderT TraceData (StateT (OndimState m) (ExceptT OndimException m)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Ondim where
  lift = Ondim . lift . lift . lift

instance MonadState s m => MonadState s (Ondim m) where
  get = lift get
  put x = lift (put x)

instance MonadReader s m => MonadReader s (Ondim m) where
  ask = lift ask
  local f (Ondim (ReaderT g)) = Ondim $ ReaderT $ local f . g

instance MonadError e m => MonadError e (Ondim m) where
  throwError = lift . throwError
  catchError (x :: Ondim m a) c =
    let f :: TraceData -> OndimState m -> m (Either OndimException (a, OndimState m))
        f r s =
          let c' e = coerce (c e) r s
           in coerce x r s `catchError` c'
     in coerce f

-- * Filters and Expansions

type GlobalConstraints m t = (OndimNode t, Typeable t, Monad m)

data DefinitionSite = CodeDefinition SrcLoc | FileDefinition FilePath | NoDefinition
  deriving (Eq, Show)

callStackSite :: DefinitionSite
callStackSite = case GHC.toList callStack of
  x : _ -> CodeDefinition (snd x)
  [] -> NoDefinition

-- Filters

type Filter m t = t -> Ondim m [t] -> Ondim m [t]
type MapFilter m t = Ondim m [t] -> Ondim m [t]
type Filters m = Map Text (SomeFilter m)

data SomeFilter m where
  SomeFilter :: TypeRep a -> Filter m a -> SomeFilter m
  SomeMapFilter :: TypeRep a -> MapFilter m a -> SomeFilter m

-- Expansions

type Expansion m t = t -> Ondim m [t]
newtype Namespace m = Namespace {getExpansions :: HashMap Text (SomeExpansion m)}
type GlobalExpansion m = forall a. GlobalConstraints m a => Expansion m a

data SomeExpansion m where
  SomeExpansion :: TypeRep a -> DefinitionSite -> Expansion m a -> SomeExpansion m
  GlobalExpansion :: DefinitionSite -> GlobalExpansion m -> SomeExpansion m
  Template :: OndimNode a => DefinitionSite -> a -> SomeExpansion m
  NamespaceData :: Namespace m -> SomeExpansion m

instance Semigroup (Namespace m) where
  (Namespace x) <> (Namespace y) = Namespace $ Map.unionWith f x y
    where
      f (NamespaceData n) (NamespaceData m) = NamespaceData $ n <> m
      f z _ = z

instance Monoid (Namespace m) where
  mempty = Namespace mempty

-- Conversions

type Conversions = forall a b. TypeRep a -> TypeRep b -> a -> b

-- * State data

-- | Ondim's expansion state
data OndimState (m :: Type -> Type) = OndimState
  { -- | Named expansions
    expansions :: Namespace m,
    -- | Similar to expansions, but are always applied after the expansion. The
    -- purpose of the name is just to facilitate binding/unbinding and control
    -- execution order, it respects lexicographic order on keys.
    filters :: Filters m
  }
  deriving (Generic)

instance Monoid (OndimState m) where
  mempty = OndimState mempty mempty

instance Semigroup (OndimState m) where
  OndimState x1 y1 <> OndimState x2 y2 = OndimState (x1 <> x2) (y1 <> y2)

-- * Exceptions

-- | Data used for debugging purposes
data TraceData = TraceData {depth :: Int, expansionTrace :: [(Text, DefinitionSite)], currentSite :: DefinitionSite}
  deriving (Eq, Show)

initialTraceData :: TraceData
initialTraceData = TraceData 0 [] NoDefinition

getCurrentSite :: Monad m => Ondim m DefinitionSite
getCurrentSite = Ondim $ asks currentSite

withSite :: Monad m => DefinitionSite -> Ondim m a -> Ondim m a
withSite site = Ondim . local (\s -> s {currentSite = site}) . unOndimT

data ExceptionType
  = MaxExpansionDepthExceeded
  | -- | Template errors are not meant to be catched from within the templates.
    -- Instead, they point at user errors that are supposed to be fixed.
    TemplateError
      CallStack
      -- ^ Call stack
      Text
      -- ^ Custom error message.
  | -- | Failures are expected in some sense.
    Failure
      SomeTypeRep
      -- ^ Type representation of the node which triggered the failure.
      Text
      -- ^ Identifier of the node which triggered the failure.
      OndimFailure
  deriving (Show, Exception)

-- | Failures related to the expansions.
data OndimFailure
  = -- | Identifier is not a bound expansion.
    NotBound
  | -- | Expansion bound under identifier has mismatched type.
    ExpansionWrongType
      SomeTypeRep
      -- ^ Type representation of the expansion that is bound under the identifier.
  | -- | Expansion bound under identifier has mismatched type.
    TemplateWrongType
      SomeTypeRep
      -- ^ Type representation of the expansion that is bound under the identifier.
  | -- | Custom failure.
    FailureOther Text
  deriving (Show, Exception)

data OndimException = OndimException ExceptionType TraceData
  deriving (Show, Exception)

catchException ::
  Monad m =>
  Ondim m a ->
  (OndimException -> Ondim m a) ->
  Ondim m a
catchException (Ondim m) f = Ondim $ catchError m (unOndimT . f)

throwException :: Monad m => ExceptionType -> Ondim m a
throwException e = do
  td <- Ondim ask
  Ondim $ throwError (OndimException e td)

throwTemplateError :: (HasCallStack, Monad m) => Text -> Ondim m a
throwTemplateError t = throwException (TemplateError callStack t)

catchFailure ::
  Monad m =>
  Ondim m a ->
  (OndimFailure -> Text -> SomeTypeRep -> TraceData -> Ondim m a) ->
  Ondim m a
catchFailure (Ondim m) f = Ondim $ catchError m \(OndimException exc tdata) ->
  case exc of
    Failure trep name e -> unOndimT $ f e name trep tdata
    _other -> m

throwExpFailure ::
  forall t m a.
  (Monad m, Typeable t) =>
  Text ->
  OndimFailure ->
  Ondim m a
throwExpFailure t f =
  throwException $ Failure (someTypeRep (Proxy @t)) t f

-- * Combinators

data OCTag

type HasSub tag ls t = HS.HasSub OCTag tag ls t
type Carrier a = HS.Carrier OCTag a
type ToSpec a = HS.ToSpec OCTag a
type ToSpecSel s a = HS.ToSpecSel OCTag s a

-- * Attributes

-- | Alias for attributes
type Attribute = (Text, Text)
