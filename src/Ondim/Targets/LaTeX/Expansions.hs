-- | 

module Ondim.Targets.LaTeX.Expansions where


import Ondim
import Ondim.Extra.Exceptions (notBoundFilter)
import Ondim.Extra.Expansions
import Ondim.Targets.LaTeX.Instances (Node (..), escapeLaTeX)

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
      "ignore" #* ignore
      "if" #* ifBound
      "any" #* anyBound
      "match" #* switchBound
      "bind" #* bind
      "scope" #* scope
      "with" #* with
      "open" #* open
      "" ## liftChildren @Node
    `bindingFilters` do
      "notBound" $# notBoundFilter @Node (== "sep")
      "sep" $* fmap (foldr go [])
         where
           go (Command "sep" _ _) (Command "sep" _ _ : xs) = Text "\n\n" : xs
           go x xs = x : xs
