module Ondim.Targets.Pandoc.Expansions where

import Ondim
import Ondim.Extra.Standard (attrSub, standardMap)
import Ondim.Targets.Pandoc.Instances ()

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` standardMap
    `bindingFilters` do
      "attrSub" $* attrSub '!' ('(', ')')
