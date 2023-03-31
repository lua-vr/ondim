module Ondim.Targets.Pandoc.Expansions where

import Data.Text qualified as T
import Ondim
import Ondim.Extra.Expansions
  ( attrSub,
    bind,
    bindText,
    ifBound,
    scope,
    switchBound,
  )
import Ondim.Targets.Pandoc.Instances ()
import Text.Pandoc.Definition
import Text.Pandoc.Walk

cons :: forall m. Monad m => Expansion m Block
cons x = do
  nodes <- liftChildren x
  pure $
    fromMaybe nodes do
      (h0 :| nodes') <- nonEmpty nodes
      (h1 :| nodes'') <- nonEmpty nodes'
      let f :: [Inline] -> [Inline]
          f y = getSubstructure @Inline h0 ++ y
      pure $ modSubstructure @Inline f h1 : nodes''

bindDefaults ::
  forall m t.
  Monad m =>
  Ondim m t ->
  Ondim m t
bindDefaults st =
  st
    `binding` do
      "if" #* ifBound
      "switch" #* switchBound
      "bind" #* bind
      "scope" #* scope
      "cons" ## cons
      "bind-text" ## bindText (stringify @Block)
      "bind-text" ## bindText (stringify @Inline)
    `bindingFilters` do
      "attrSub" $# attrSub

-- Miscellaneous (from Text.Pandoc.Shared)

stringify :: Walkable Inline a => a -> T.Text
stringify = query go
  where
    go :: Inline -> T.Text
    go Space = " "
    go SoftBreak = " "
    go (Str x) = x
    go (Code _ x) = x
    go (Math _ x) = x
    go LineBreak = " "
    go _ = ""
