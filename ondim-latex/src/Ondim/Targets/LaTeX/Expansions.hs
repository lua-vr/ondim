module Ondim.Targets.LaTeX.Expansions where

import Ondim
import Ondim.Extra.Expansions
import Ondim.Extra.Standard (standardMap)
import Ondim.Targets.LaTeX.Instances (LaTeXNode (..), escapeLaTeX)

defaultState :: OndimState s
defaultState =
  OndimState
    { expansions = exps
    }
  where
    exps = mapToNamespace do
      "escaped.tex" ## \(node :: LaTeXNode) -> do
        t <- lookupAttr' "text" node
        return [Text $ escapeLaTeX t]
      standardMap
