{- | Here we define a "standard library" of useful expansions. Examples in the
   Haddocks are given for HTML, but they work anagously for any supported
   format.
-}
module Ondim.Extra.Standard
  ( standardMap,
    bind,
    call,
    open,
    with,
    ifBound,
    anyBound,
    matchBound,
    ignore,
    scope,
  ) where

import Data.Text qualified as T
import Ondim
import Ondim.Debug
import Ondim.Extra.Exceptions (tryExp)
import Ondim.Extra.Expansions

-- | Namespace including all expansions defined in this module.
standardMap :: NamespaceMap m
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

{- | This expansion expands its children depending on whether all the specified
   expansions are bound or not.

   > <e:bind id=joseph/>
   >
   > <e:if id="joseph,maria">
   >   'joseph' AND 'maria' are bound
   >   <e:else>
   >     'joseph' OR 'maria' are not bound
   >   </e:else>
   > </e:if>
-}
ifBound :: PolyExpansion m
ifBound node = do
  attrs <- T.split (== ',') <$> getSingleAttr' "id" node
  bound <- allM exists attrs
  ifElse bound node
  where
    exists n = isJust . lookup n . expansions <$> getOndimS

{- | This expansion expands its children depending on whether any of the specified
   expansions are bound or not.

   > <e:bind id=joseph/>
   >
   > <e:any id="joseph,maria">
   >   'joseph' OR 'maria' are bound
   >   <e:else>
   >     'joseph' AND 'maria' are not bound
   >   </e:else>
   > </e:any>
-}
anyBound :: PolyExpansion m
anyBound node = do
  attrs <- fst <<$>> attributes node
  bound <- anyM exists attrs
  ifElse bound node
  where
    exists n = isJust . lookup n . expansions <$> getOndimS

{- | This expansion allows you to case match on the (textual) value of another
   expansion, or return a (optional) default clause.

   > <e:bind id=name>joseph</e:bind>
   >
   > <e:match id=name>
   >   <e:case maria>A dress</e:case>
   >   <e:case joseph>A nice hat</e:case>
   >   <e:default>Some shoes</e:default>
   > </e:any>
-}
matchBound :: PolyExpansion m
matchBound node = do
  tag <- getSingleAttr' "id" node
  tagC <- getText tag
  switchWithDefault (rightToMaybe tagC) node

{- | This expansion always return an empty list of nodes. It may be used for
   comments that should not be rendered in the output format.

   > <!-- I'll be rendered as a normal HTML comment -->
   > <e:ignore>But I'll not be rendered in the output at all.</e:ignore>
-}
ignore :: PolyExpansion m
ignore = const $ pure []

{- | This expansion renames other expansions inside of it.

   > <e:bind id=hello>Hello,</e:bind>
   > <e:bind id=world>world.</e:bind>
   >
   > <e:with new-hello="hello" new-world="world">
   >   <e:new-hello /> <e:new-world />
   > <e:with/>
-}
open :: PolyExpansion m
open node = do
  name <- getSingleAttr' "id" node
  exps <- getNamespace name
  withoutExpansions [name] $
    case exps of
      Right n -> withNamespace n $ expandChildren node
      Left e -> throwExpFailure @() name e

{- | This expansion renames other expansions inside of it.

   > <e:bind id=hello>Hello,</e:bind>
   > <e:bind id=world>world.</e:bind>
   >
   > <e:with new-hello="hello" new-world="world">
   >   <e:new-hello /> <e:new-world />
   > <e:with/>
-}
with :: PolyExpansion m
with node = do
  exps <- expansions <$> getOndimS
  actions <-
    attributes node <&> map \(k, v) ->
      let expansion = lookup v exps
       in withoutExpansions [v] . withSomeExpansion k expansion
  foldr ($) (expandChildren node) actions

{- | This expansion creates a new scope for the its children, in the sense that
 the inner state does not leak outside.

  For this reason, it can be used to call other expansions with "arguments":

   > <e:bind id=animal-entry>There is a <e:animal /> with age <e:age /></e:bind>
   >
   > <e:scope>
   >   <e:bind id=animal>Lion</e:bind>
   >   <e:bind id=age>9 years</e:bind>
   >   <e:animal-entry />
   > <e:scope/>
   >
   > <e:animal /> <!-- "not bound" error! -->
-}
scope :: PolyExpansion m
scope node = do
  s <- getOndimS
  expandChildren node <* putOndimS s

{- | This expansion calls other expansions.

   > <e:bind id=test>
   >   Hello, world. <e:caller.children />
   > </e:bind>
   > <e:call id=test>
   >   How are you doing?
   > </e:call>
-}
call :: PolyExpansion m
call node = do
  name <- getSingleAttr' "id" node
  callExpansion name node

{- | This expansion adds the content of the node's children to the state as a
  template. If you're familiar with Heist, it's like Heist's @bind@ splice.

   > <e:bind id=greet>
   >   Hello, <e:caller.attrs.name />.
   > </e:bind>
   >
   > <e:greet name="Joseph" />
-}
bind :: (HasCallStack) => PolyExpansion m
bind node = do
  attrs <- attributes node
  defSite <- getCurrentSite
  let strict = any (("strict" ==) . fst) attrs
  thing <-
    if strict
      then expandChildren node
      else return $ children node
  case getSingleAttr "id" attrs of
    Just name -> do
      putSomeExpansion name . Just $ TemplateData defSite thing
      pure []
    Nothing -> throwTemplateError "No name for expansion"
