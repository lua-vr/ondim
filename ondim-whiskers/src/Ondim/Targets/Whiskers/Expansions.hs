module Ondim.Targets.Whiskers.Expansions where

import Ondim
import Ondim.Extra.Standard (standardMap)

defaultState :: OndimState s
defaultState =
  OndimState
    { expansions = exps
    }
  where
    exps = mapToNamespace standardMap
