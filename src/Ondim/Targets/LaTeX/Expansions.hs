module Ondim.Targets.LaTeX.Expansions where

import Ondim
import Ondim.Extra.Expansions
import Ondim.Extra.Standard (standardMap)
import Ondim.Targets.LaTeX.Instances (Node (..), escapeLaTeX, renderLaTeX)
import Ondim.Targets.LaTeX.Parser (parseLaTeX)

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      "escaped" ## \(node :: Node) -> do
        t <- lookupAttr' "text" node
        return [Text $ escapeLaTeX t]
      standardMap
      "" ## liftChildren @Node
    `bindingFilters` do
      "sep" $* fmap (foldr go [])
  where
    go (Command "sep" _ _) (Command "sep" _ _ : xs) = Text "\n\n" : xs
    go x xs = x : xs

expandLaTeX ::
  forall m a.
  HasCallStack =>
  GlobalConstraints m a =>
  Expansion m a
expandLaTeX node = do
  text <- lookupAttr' "text" node
  let parsed = parseLaTeX "expand.latex input" text
  either (throwTemplateError . toText) convert parsed
  where
    noCast = throwTemplateError "target is missing cast from text!"
    convert x = do
      case ondimCast @Text of
        Just cast -> cast . renderLaTeX <$> liftNodes x
        Nothing -> noCast
