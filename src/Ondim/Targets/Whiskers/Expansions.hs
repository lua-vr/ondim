module Ondim.Targets.Whiskers.Expansions where

import Ondim
import Ondim.Extra.Exceptions (tryAttrFilter)
import Ondim.Extra.Expansions (lookupAttr')
import Ondim.Extra.Standard (attrSub, standardMap)
import Ondim.Targets.Whiskers.Instances (Node, renderWhiskers)
import Ondim.Targets.Whiskers.Parser (parseWhiskers)

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

expandWhiskers ::
  forall m a.
  HasCallStack =>
  GlobalConstraints m a =>
  (Text, Text) ->
  Expansion m a
expandWhiskers delims node = do
  text <- lookupAttr' "text" node
  let parsed = parseWhiskers delims "expand.whiskers input" text
  case parsed of
    Left e -> throwTemplateError $ toText e
    Right p
      | Just fT <- fromText -> fT . renderWhiskers <$> liftNodes p
      | otherwise -> throwTemplateError "expand.whiskers needs a fromText instance!"
