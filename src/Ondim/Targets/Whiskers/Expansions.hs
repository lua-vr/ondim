module Ondim.Targets.Whiskers.Expansions where

import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter)
import Ondim.Extra.Standard (attrSub, standardMap)
import Ondim.Targets.Whiskers.Instances (Node)

defaultState :: Monad m => OndimState m
defaultState =
  OndimState { expansions = exps,
               filters = filts
             }
  where
    exps = mapToNamespace do
      standardMap
      "" ## liftChildren @Node
    filts = mapToFilters do
      "attrSub" $* attrSub '$' ('{', '}')
      "tryAttr" $# tryAttrFilter
