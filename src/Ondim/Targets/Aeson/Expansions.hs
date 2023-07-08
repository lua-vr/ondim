module Ondim.Targets.Aeson.Expansions
  ( bindDefaults,
  ) where

import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter)
import Ondim.Extra.Standard (attrSub, standardMap)
import Ondim.Targets.Aeson.Instances ()

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` standardMap
    `bindingFilters` do
      "attrSub" $* attrSub '$' ('{', '}')
      "tryAttr" $# tryAttrFilter
