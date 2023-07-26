{- | This module defines a user-friendly API over the core functionality
 (implemented in Ondim.MultiWalk.Core).
-}
module Ondim
  ( -- * Classes
    OndimNode (..),
    ondimCast,

    -- * Combinators
    module Ondim.MultiWalk.Combinators,

    -- * Data types
    GlobalConstraints,
    Expansion,
    GlobalExpansion,
    SomeExpansion,
    Namespace,
    OndimState (..),

    -- * Monad
    Ondim,
    runOndimTWith,
    evalOndimTWith,
    evalOndimT,

    -- * Rendering
    renderNodeOrError,

    -- * Exceptions
    TraceData (..),
    ExceptionType (..),
    OndimException (..),
    withoutNBErrors,
    withNBErrors,
    throwTemplateError,
    throwException,
    catchException,
    OndimFailure (..),
    throwExpFailure,
    catchFailure,
    DefinitionSite (..),
    getCurrentSite,
    fileSite,
    callStackSite,

    -- * State transformations
    module Ondim.MultiWalk.State,
    -- Get parts of the state
    getExpansion,
    getTemplate,
    getNamespace,
    getTemplate',
    getText,
    getText',
    -- Calling
    callExpansion,
    callTemplate,
    callText,

    -- * Node lifting
    liftNode,
    liftNodes,
    modSubLift,
    liftSubstructures,

    -- * Structure
    getSubstructure,
    liftChildren,
    lookupAttr,

    -- * Auxiliary
    Attribute,
  )
where

import Data.List qualified as L
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import Ondim.MultiWalk.Combinators
import Ondim.MultiWalk.Core
import Ondim.MultiWalk.State
import Ondim.MultiWalk.Substructure
import Prelude hiding (All)

-- | Runs the Ondim action with a given initial state.
evalOndimTWith ::
  Monad m =>
  OndimState m ->
  Ondim m a ->
  m (Either OndimException a)
evalOndimTWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialTraceData
      `evalStateT` s

runOndimTWith ::
  Monad m =>
  OndimState m ->
  Ondim m a ->
  m (Either OndimException (a, OndimState m))
runOndimTWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialTraceData
      `runStateT` s

-- | Runs the Ondim action with empty initial state.
evalOndimT :: Monad m => Ondim m a -> m (Either OndimException a)
evalOndimT = evalOndimTWith mempty

-- Children

liftChildren ::
  forall t m.
  GlobalConstraints m t =>
  Expansion m t
liftChildren = liftNodes . children

renderNodeOrError :: (HasCallStack, Monad m) => OndimNode a => a -> Ondim m LByteString
renderNodeOrError =
  case renderNode of
    Just render -> return . render
    Nothing -> const $ throwTemplateError "This type cannot be rendered."

-- Attributes

lookupAttr ::
  (Monad m, OndimNode t) =>
  Text ->
  t ->
  Ondim m (Maybe Text)
lookupAttr key = fmap (L.lookup key) . attributes

-- | Either applies template 'name', or throws an error if it does not exist.
callTemplate :: forall t m. GlobalConstraints m t => Text -> Ondim m [t]
callTemplate name = do
  exps <- getTemplate name
  either (throwExpFailure @t name) return exps

-- | Either applies template 'name', or throws an error if it does not exist.
callText :: Monad m => Text -> Ondim m Text
callText name = do
  exps <- getText name
  either (throwExpFailure @Text name) return exps

-- | Either applies expansion 'name', or throws an error if it does not exist.
callExpansion :: forall t m. GlobalConstraints m t => Text -> Expansion m t
callExpansion name arg = do
  exps <- getExpansion name
  either (throwExpFailure @t name) ($ arg) exps
