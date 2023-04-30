module Ondim.Targets.Whiskers.Expansions where

import Ondim
import Ondim.Extra.Expansions
import Ondim.Targets.Whiskers.Instances (Node)
import Ondim.Extra.Exceptions (mbAttrFilter, notBoundFilter)

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      "o" #. do
        "ignore" #* ignore
        "if" #* ifBound
        "match" #* switchBound
        "bind" #* bind
        "scope" #* scope
        "with" #* with
        "open" #* open
      "" ## liftChildren @Node
      "@try" ## const $ pure ([] :: [Attribute])
    `bindingFilters` do
      "attrSub" $# attrSub
      "mbAttr" $# mbAttrFilter
      "notBound" $# notBoundFilter @Node (const False)
