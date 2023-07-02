module Ondim.Targets.Whiskers.Expansions where

import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter)
import Ondim.Extra.Standard (attrSub, standardMap)
import Ondim.Targets.Whiskers.Instances (Node)

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      standardMap
      "" ## liftChildren @Node
    `bindingFilters` do
      "attrSub" $* attrSub
      "tryAttr" $# tryAttrFilter
