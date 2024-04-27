module Ondim.Debug
    ( -- * Debug data
      TraceData (..),
      ExceptionType (..),
      OndimException (..),
      OndimFailure (..),

      -- * Disabling errors
      withoutNBErrors,
      withNBErrors,

      -- * Throwing and catching errors
      throwTemplateError,
      throwException,
      catchException,
      throwExpFailure,
      catchFailure,

      -- * Definition sites
      DefinitionSite (..),
      getCurrentSite,
      withSite,
      fileSite,
      callStackSite,
    ) where

import Ondim.Internal.Core
import Ondim.Internal.Basic
