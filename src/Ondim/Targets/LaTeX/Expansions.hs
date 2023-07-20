module Ondim.Targets.LaTeX.Expansions where

import Ondim
import Ondim.Extra.Expansions
import Ondim.Extra.Standard (standardMap)
import Ondim.Targets.LaTeX.Instances (Node (..), escapeLaTeX)

defaultState :: Monad m => OndimState m
defaultState =
  OndimState
    { expansions = exps,
      filters = filts
    }
  where
    exps = mapToNamespace do
      "escaped.tex" ## \(node :: Node) -> do
        t <- lookupAttr' "text" node
        return [Text $ escapeLaTeX t]
      standardMap
      "sep.tex" #* pure . one
    filts = mapToFilters do
      "sep.tex" $* fmap (foldr go [])
    go (Command "sep" _ _) (Command "sep" _ _ : xs) = Text "\n\n" : xs
    go x xs = x : xs
