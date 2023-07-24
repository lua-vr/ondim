module Ondim.Targets.Whiskers.Expansions where

import Ondim
import Ondim.Extra.Standard (standardMap)

defaultState :: Monad m => OndimState m
defaultState =
  OndimState
    { expansions = exps,
      filters = mempty
    }
  where
    exps = mapToNamespace standardMap
