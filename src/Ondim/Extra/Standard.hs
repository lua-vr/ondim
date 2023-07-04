module Ondim.Extra.Standard
  ( standardMap,
    ifBound,
    anyBound,
    matchBound,
    ignore,
    open,
    with,
    scope,
    call,
    bind,
    bindText,
    attrSub,
  ) where

import Data.Attoparsec.Text (Parser, char, string, takeTill)
import Ondim
import Ondim.Extra.Expansions
import Replace.Attoparsec.Text (streamEditT)
import Ondim.Extra.Exceptions (tryExp)

standardMap :: ExpansionMap m
standardMap = do
  -- Control flow
  "if" #* ifBound
  "any" #* anyBound
  "match" #* matchBound
  "try" #* tryExp
  "ignore" #* ignore
  -- Environment
  "open" #* open
  "with" #* with
  "scope" #* scope
  -- Calling
  "call" #* call
  -- Binding
  "bind" #* bind

-- * Control flow

ifBound :: forall t m. GlobalConstraints m t => Expansion m t
ifBound node = do
  attrs <- fst <<$>> attributes node
  bound <- allM exists attrs
  ifElse bound node
  where
    exists n = isJust . lookupExpansion n . expansions <$> getOndimS

anyBound :: forall t m. GlobalConstraints m t => Expansion m t
anyBound node = do
  attrs <- fst <<$>> attributes node
  bound <- anyM exists attrs
  ifElse bound node
  where
    exists n = isJust . lookupExpansion n . expansions <$> getOndimS

matchBound :: GlobalExpansion m
matchBound node = do
  tag <- getSingleAttr' "exp" node
  tagC <- getTextData tag
  switchWithDefault (rightToMaybe tagC) node

ignore :: forall t m. Monad m => Expansion m t
ignore = const $ pure []

-- * Environment

open :: GlobalExpansion m
open node = do
  name' <- viaNonEmpty (fst . head) <$> attributes node
  name <- maybe (throwTemplateError "Namespace not provided.") pure name'
  exps <- getNamespace name
  withoutExpansions [name] $
    case exps of
      Right n -> withNamespace n $ liftChildren node
      Left e -> throwExpFailure @() name e

with :: GlobalExpansion m
with node = do
  exps <- expansions <$> getOndimS
  actions <-
    attributes node <&> map \(k, v) ->
      let expansion = lookupExpansion v exps
       in withoutExpansions [v] . withSomeExpansion k expansion
  foldr ($) (liftChildren node) actions

{- | This expansion creates a new scope for the its children, in the sense that
 the inner state does not leak outside.

  For this reason, it can be used to call other expansions with "arguments":

   > <bind animal-entry>There is a <animal /> with age <age /></bind>
   >
   > <scope>
   >   <bind animal>Lion</bind>
   >   <bind age>9 years</bind>
   >   <animal-entry />
   > <scope/>
-}
scope :: forall t m. GlobalConstraints m t => Expansion m t
scope node = do
  s <- getOndimS
  liftChildren node <* putOndimS s

-- * Calling

call :: GlobalExpansion m
call node = do
  attrs <- attributes node
  (name, _rest) <-
    case nonEmpty attrs of
      Just (name :| rest) -> return (fst name, rest)
      Nothing -> throwTemplateError "Expansion name not provided."
  callExpansion name node

-- * Binding

-- | This expansion works like Heist's `bind` splice
bind :: HasCallStack => GlobalExpansion m
bind node = do
  attrs <- attributes node
  defSite <- getCurrentSite
  let strict = any (("strict" ==) . fst) attrs
  thing <-
    if strict
      then do
        child <- liftChildren node
        return $ pure child
      else return $ liftChildren node
  case getSingleAttr "name" attrs of
    Just name -> do
      putSomeExpansion name $ someExpansion' defSite $ \inner -> do
        callSite <- getCurrentSite
        attrs' <- attributes inner
        withSite defSite $
          thing `binding` do
            "caller" #. do
              "children" ## const (withSite callSite $ liftChildren inner)
              "attrs" #. assocsExp textData attrs'
      pure []
    Nothing -> throwTemplateError "No name for expansion"

{- | This expansion works like Heist's `bind` splice, but binds what's inside as
  text (via the toTxt parameter).
-}
bindText ::
  HasCallStack =>
  GlobalConstraints m t =>
  (t -> Text) ->
  Expansion m t
bindText toTxt self = do
  attrs <- attributes self
  defSite <- getCurrentSite
  let strict = any (("strict" ==) . fst) attrs
  thing <-
    if strict
      then do
        child <- liftChildren self
        return $ pure child
      else return $ liftChildren self
  case getSingleAttr "name" attrs of
    Just name -> do
      putSomeExpansion name $
        textMData' defSite $
          withSite defSite $
            foldMap toTxt <$> thing
      pure []
    Nothing -> throwTemplateError "No name for expansion"

-- * String interpolation

-- | Substitution of !(name) in attribute text
interpParser :: Parser Text
interpParser = do
  _ <- string "!("
  s <- takeTill (== ')')
  _ <- char ')'
  pure s

attrEdit :: Monad m => Text -> Ondim m Text
attrEdit = streamEditT interpParser callTextData

attrSub :: Monad m => MapFilter m Text
attrSub t = mapM attrEdit =<< t
