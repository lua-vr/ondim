-- | 

module Ondim.Targets.LaTeX.Expansions where


import Ondim
import Ondim.Extra.Exceptions (tryFilter)
import Ondim.Extra.Expansions
import Ondim.Targets.LaTeX.Instances (Node (..), escapeLaTeX)
import Ondim.Extra.Standard (standardMap)

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
      "notBound" $# tryFilter @Node
      "sep" $* fmap (foldr go [])
         where
           go (Command "sep" _ _) (Command "sep" _ _ : xs) = Text "\n\n" : xs
           go x xs = x : xs
