module Ondim.Targets.Whiskers.Expansions where

import Ondim
import Ondim.Extra.Exceptions (mbAttrFilter, notBoundFilter)
import Ondim.Extra.Expansions
import Ondim.Targets.Whiskers.Instances (Node)

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      "ignore" #* ignore
      "if" #* ifBound
      "any" #* anyBound
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
