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
type Filters m = Map Text (SomeFilter m)
type GlobalFilter m = forall a. GlobalConstraints m a => Filter m a

data SomeFilter m where
  SomeFilter :: TypeRep a -> Filter m a -> SomeFilter m
  GlobalFilter :: GlobalFilter m -> SomeFilter m

-- Expansions

type Expansion m t = t -> Ondim m [t]
newtype Expansions m = Expansions {getExpansions :: HashMap Text (SomeExpansion m)}
type GlobalExpansion m = forall a. GlobalConstraints m a => Expansion m a

data SomeExpansion m where
  SomeExpansion :: TypeRep a -> DefinitionSite -> Expansion m a -> SomeExpansion m
  GlobalExpansion :: DefinitionSite -> GlobalExpansion m -> SomeExpansion m
  TextData :: DefinitionSite -> Text -> SomeExpansion m
  Namespace :: Expansions m -> SomeExpansion m

instance Semigroup (Expansions m) where
  (Expansions x) <> (Expansions y) = Expansions $ Map.unionWith f x y
    where
      f (Namespace n) (Namespace m) = Namespace $ n <> m
      f z _ = z

instance Monoid (Expansions m) where
  mempty = Expansions mempty

-- * State data

-- | Ondim's expansion state
data OndimState (m :: Type -> Type) = OndimState
  { -- | Named expansions
    expansions :: Expansions m,
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
data TraceData = TraceData {depth :: Int, expansionTrace :: [Text], currentSite :: DefinitionSite}
  deriving (Eq, Show)

initialTraceData :: TraceData
initialTraceData = TraceData 0 [] NoDefinition

getCurrentSite :: Monad m => Ondim m DefinitionSite
getCurrentSite = Ondim $ asks currentSite

withSite :: Monad m => DefinitionSite -> Ondim m a -> Ondim m a
withSite site = Ondim . local (\s -> s {currentSite = site}) . unOndimT

data ExceptionType
  = MaxExpansionDepthExceeded
  | TemplateError CallStack Text
  | ExpansionNotBound SomeTypeRep Text
  deriving (Show, Exception)

data OndimException = OndimException ExceptionType TraceData
  deriving (Show, Exception)

throwOndim :: Monad m => ExceptionType -> Ondim m a
throwOndim e = do
  td <- Ondim ask
  Ondim $ throwError (OndimException e td)

catchOndim :: Monad m => Ondim m a -> (OndimException -> Ondim m (Maybe a)) -> Ondim m a
catchOndim (Ondim m) c = Ondim $ catchError m \e ->
  maybe (throwError e) pure =<< unOndimT (c e)

throwTemplateError :: (HasCallStack, Monad m) => Text -> Ondim m a
throwTemplateError t = throwOndim (TemplateError callStack t)

throwNotBound :: forall t m a. (Monad m, Typeable t) => Text -> Ondim m a
throwNotBound t = throwOndim $ ExpansionNotBound (someTypeRep (Proxy @t)) t

-- * Combinators

data OCTag

type HasSub tag ls t = HS.HasSub OCTag tag ls t
type Carrier a = HS.Carrier OCTag a
type ToSpec a = HS.ToSpec OCTag a
type ToSpecSel s a = HS.ToSpecSel OCTag s a

-- * Attributes

-- | Alias for attributes
type Attribute = (Text, Text)
