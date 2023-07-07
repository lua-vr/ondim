module Ondim.Targets.Aeson.Expansions
  ( bindDefaults,
  ) where

import Data.Aeson (Value (..))
import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter)
import Ondim.Extra.Standard (attrSub, bindText, standardMap)
import Ondim.Targets.Aeson.Instances ()

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      standardMap
      "bind-text" ## bindText valueToText
    `bindingFilters` do
      "attrSub" $* attrSub '$' ('{', '}')
      "tryAttr" $# tryAttrFilter
  where
    valueToText (String t) = t
    valueToText _ = mempty
