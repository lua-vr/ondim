module Ondim.Targets.Aeson.Expansions where

import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter)
import Ondim.Extra.Standard (attrSub, standardMap)
import Ondim.Targets.Aeson.Instances ()

defaultState :: Monad m => OndimState m
defaultState =
  OndimState { expansions = exps,
               filters = filts
             }
  where
    exps = mapToNamespace standardMap
    filts = mapToFilters do
      "attrSub" $* attrSub '$' ('{', '}')
      "tryAttr" $# tryAttrFilter
