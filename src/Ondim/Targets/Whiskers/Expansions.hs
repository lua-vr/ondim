module Ondim.Targets.Whiskers.Expansions where

import Ondim
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
      "o" #. do
        "ignore" #* ignore
        "if" #* ifBound
        "match" #* switchBound
        "bind" #* bind
        "scope" #* scope
        "with" #* with
        "open" #* open
        "debug" #* debug
      "" ## liftChildren @Node
      "@try" ## const $ pure ([] :: [Attribute])
    `bindingFilters` do
      "attrSub" $# attrSub
      "mbAttr" $# mbAttrFilter
      "notBound" $# notBoundFilter @Node mempty
