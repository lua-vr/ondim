module Ondim.Targets.HTML.Expansions where

import Ondim
import Ondim.Extra.Expansions (attrSub, bind, bindText, ifBound, ignore, prefixed, scope, switchBound, with)
import Ondim.Targets.HTML.Instances

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim HtmlTag m t ->
  Ondim HtmlTag m t
bindDefaults st =
  st
    `binding` prefixed "o:" do
      "ignore" ## ignore @HtmlNode
      "if" ## ifBound
      "match" ## switchBound
      "bind" ## bind
      "scope" ## scope
      "bind-text" ## bindText nodeText
      "with" ## with
    `bindingFilters` do
      "attrSub" ## attrSub
