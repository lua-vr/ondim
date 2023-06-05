-- | 

module Ondim.Targets.LaTeX.Expansions where


import Ondim
import Ondim.Extra.Exceptions (notBoundFilter)
import Ondim.Extra.Expansions
import Ondim.Targets.LaTeX.Instances (Node (..))

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      "raw" ## \(node :: Node) -> do
        name <- lookupAttr' "name" node
        t' <- callText name
        return [Text t']
      "ignore" #* ignore
      "if" #* ifBound
      "any" #* anyBound
      "match" #* switchBound
      "bind" #* bind
      "scope" #* scope
      "with" #* with
      "open" #* open
      "" ## liftChildren @Node
      "@try" ## const $ pure ([] :: [Attribute])
    `bindingFilters` do
      "notBound" $# notBoundFilter @Node (const False)
