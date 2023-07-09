module Ondim.Targets.Pandoc.Expansions where

import Ondim
import Ondim.Extra.Standard (attrSub, standardMap)
import Ondim.Targets.Pandoc.Instances ()

defaultState :: Monad m => OndimState m
defaultState =
  OndimState { expansions = exps,
               filters = filts
             }
  where
    exps = mapToNamespace standardMap
    filts = mapToFilters do
      "attrSub" $* attrSub '!' ('(', ')')
