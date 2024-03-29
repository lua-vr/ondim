{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Ondim.MultiWalk.Basic where

import Control.Monad.Except (MonadError (..))
import Control.Monad.ST
import Control.MultiWalk.HasSub qualified as HS
import Data.HashMap.Strict qualified as Map
import Data.STRef
import Data.Text qualified as T
import GHC.Exception (SrcLoc)
import GHC.Exts qualified as GHC
import GHC.IO (ioToST)
import {-# SOURCE #-} Ondim.MultiWalk.Class
import {-# SOURCE #-} Ondim.MultiWalk.Combinators (Carrier)
import System.FilePath (takeExtensions)
import Type.Reflection (SomeTypeRep, someTypeRep)

-- * Monad

newtype Ondim s a = Ondim
  { unOndimT ::
      ReaderT
        (TraceData, STRef s (OndimState s))
        (ExceptT OndimException (ST s))
        a
  }
  deriving newtype (Functor, Applicative, Monad)

liftST :: ST s a -> Ondim s a
liftST = Ondim . lift . lift

instance MonadIO (Ondim RealWorld) where
  liftIO m = liftST $ ioToST m

-- * Filters and Expansions

data DefinitionSite
  = CodeDefinition SrcLoc
  | FileDefinition {definitionPath :: FilePath, definitionExt :: Text}
  | NoDefinition
  deriving (Eq, Show, Generic)

fileSite :: FilePath -> DefinitionSite
fileSite fp = FileDefinition fp exts
  where
    exts = T.drop 1 $ toText $ takeExtensions fp

callStackSite :: DefinitionSite
callStackSite = case GHC.toList callStack of
  x : _ -> CodeDefinition (snd x)
  [] -> NoDefinition

-- Expansions

-- | An expansion.
type Expansion s t = t -> Ondim s [t]

{- | A namespace. Internally represented as a hashmap from 'Text' keys to
   @'NamespaceItem' m@ values.
-}
newtype Namespace m = Namespace {hashmap :: HashMap Text (NamespaceItem m)}
  deriving (Generic)

-- | An expansion that is polymorphic on the type.
type PolyExpansion s = forall a. (OndimNode a) => Expansion s a

{- | An opaque datatype that should be regarded as a sum of four possible types:

  1. Typed expansions, i.e., expansions that apply to a single type (use the
  'Ondim.State.typedExpansion' constructor).

  2. Polymorphic expansions, i.e., expansions that are polymophic over types
  with 'OndimNode' instances (use the 'Ondim.State.polyExpansion'
  constructor).

  3. Templates, i.e., raw node data that represents templates. (use the
  'Ondim.State.templateData' constructor).

  4. Namespaces, i.e., nested namespaces. (use the 'Ondim.State.namespace'
  constructor).
-}
data NamespaceItem s where
  TypedExpansion :: (Typeable a) => DefinitionSite -> Expansion s a -> NamespaceItem s
  PolyExpansion :: DefinitionSite -> PolyExpansion s -> NamespaceItem s
  TemplateData :: (OndimNode a) => DefinitionSite -> a -> NamespaceItem s
  NamespaceData :: Namespace s -> NamespaceItem s

instance Semigroup (Namespace s) where
  (Namespace x) <> (Namespace y) = Namespace $ Map.unionWith f x y
    where
      f (NamespaceData n) (NamespaceData m) = NamespaceData $ n <> m
      f z _ = z

instance Monoid (Namespace s) where
  mempty = Namespace mempty

-- * State data

-- | Ondim's expansion state
newtype OndimState (s :: Type) = OndimState
  { -- | Named expansions
    expansions :: Namespace s
  }
  deriving (Generic)
  deriving newtype (Semigroup, Monoid)

-- * Exceptions

-- | Data used for debugging purposes
data TraceData = TraceData
  { depth :: Int,
    expansionTrace :: [(Text, DefinitionSite)],
    currentSite :: DefinitionSite,
    inhibitErrors :: Bool
  }
  deriving (Eq, Show)

initialTraceData :: TraceData
initialTraceData = TraceData 0 [] NoDefinition False

getCurrentSite :: Ondim s DefinitionSite
getCurrentSite = Ondim $ asks (currentSite . fst)

withSite :: DefinitionSite -> Ondim s a -> Ondim s a
withSite site = Ondim . local (first \s -> s {currentSite = site}) . unOndimT

data ExceptionType
  = MaxExpansionDepthExceeded
  | -- | Template errors are not meant to be catched from within the templates.
    -- Instead, they point at user errors that are supposed to be fixed.
    TemplateError
      -- | Call stack
      CallStack
      -- | Custom error message.
      Text
  | -- | Failures are expected in some sense.
    Failure
      -- | Type representation of the node which triggered the failure.
      SomeTypeRep
      -- | Identifier of the node which triggered the failure.
      Text
      OndimFailure
  deriving (Show, Exception)

-- | Failures related to the expansions.
data OndimFailure
  = -- | Identifier is not a bound expansion.
    NotBound
  | -- | Expansion bound under identifier has mismatched type.
    ExpansionWrongType
      -- | Type representation of the expansion that is bound under the identifier.
      SomeTypeRep
  | -- | Expansion bound under identifier has mismatched type.
    TemplateWrongType
      -- | Type representation of the expansion that is bound under the identifier.
      SomeTypeRep
  | -- | Custom failure.
    FailureOther Text
  deriving (Show, Exception)

data OndimException = OndimException ExceptionType TraceData
  deriving (Show, Exception)

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

-- * Combinators

data OCTag

type HasSub tag ls t = HS.HasSub OCTag tag ls t
type ToSpec a = HS.ToSpec OCTag a
type ToSpecSel s a = HS.ToSpecSel OCTag s a
type instance HS.Carrier OCTag a = Carrier a

-- * Attributes

-- | Alias for attributes
type Attribute = (Text, Text)
