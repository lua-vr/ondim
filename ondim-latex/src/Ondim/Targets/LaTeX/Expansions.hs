module Ondim.Targets.LaTeX.Expansions where

import Ondim
import Ondim.Extra.Expansions
import Ondim.Extra.Standard (standardMap)
import Ondim.Targets.LaTeX.Instances (Node (..), escapeLaTeX)

defaultState :: OndimState s
defaultState =
  OndimState
    { expansions = exps
    }
  where
    exps = mapToNamespace do
      "escaped.tex" ## \(node :: Node) -> do
        t <- lookupAttr' "text" node
        return [Text $ escapeLaTeX t]
      standardMap
