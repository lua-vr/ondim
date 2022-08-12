{-# LANGUAGE UndecidableInstances #-}
module Ondim.Pandoc where
import Ondim
import Ondim.Extra
import Text.Pandoc.Definition
import qualified Data.Text as T
import Data.HList.ContainsType
import Data.Map.Syntax ((##))
import Text.Pandoc.Walk

data PandocTag (m :: Type -> Type)

instance Monad m => OndimTag (PandocTag m) where
  type OndimTypes (PandocTag m) =
    '[ Inline
     , [Inline]
     , Block
     , [Block]
     , ([Inline], [[Block]])
     , Attribute
     , ExpansibleText
     ]
  type OndimMonad (PandocTag m) = m

instance
  ( ContainsOndimS (PandocTag m) [t]
  , ContainsType [t] (OndimTypes (PandocTag m))
  , OndimNode (PandocTag m) t
  ) => OndimNode (PandocTag m) [t] where
  type ExpTypes [t] = '[t]

instance Monad m => OndimNode (PandocTag m) Block where
  type ExpTypes Block =
    '[ Inline
     , [Inline]
     , Block
     , [Block]
     , ([Inline], [[Block]])
     , Attribute
     , ExpansibleText
     ]
  identify (Div (n,_,_) _) = Just n
  identify _ = Nothing
  validIdentifiers = Just []

instance HasSub (PandocTag m) Block Inline
instance HasSub (PandocTag m) Block [Inline]
instance HasSub (PandocTag m) Block Block
instance HasSub (PandocTag m) Block ([Inline], [[Block]])
instance HasSub (PandocTag m) Block [Block]
deriving via (NestedSub Attr Attribute) instance HasSub (PandocTag m) Block Attribute
deriving via (OneSub ExpansibleText) instance HasSub (PandocTag m) Block ExpansibleText

instance Monad m => OndimNode (PandocTag m) Inline where
  type ExpTypes Inline = '[Inline, Block, Attribute, ExpansibleText]
  identify (Span (n,_,_) _) = Just n
  identify _ = Nothing
  fromText = Just Str
  validIdentifiers = Just []

instance HasSub (PandocTag m) Inline Inline
instance HasSub (PandocTag m) Inline Block
deriving via (NestedSub Attr Attribute) instance HasSub (PandocTag m) Inline Attribute
deriving via (OneSub ExpansibleText) instance HasSub (PandocTag m) Inline ExpansibleText

instance Monad m => OndimNode (PandocTag m) ([Inline], [[Block]]) where
  type ExpTypes ([Inline], [[Block]]) = '[Inline, [Block]]
  identify (_, (x : _)) = identify @(PandocTag m) x
  identify _ = Nothing

instance HasSub (PandocTag m) ([Inline], [[Block]]) Inline
instance HasSub (PandocTag m) ([Inline], [[Block]]) [Block]

instance HasSub (PandocTag m) Attr Attribute where
  getSubs (x,y,z) = ("id", x) : ("class", T.intercalate " " y) : z
  setSubs _ = foldMap go
    where go ("id", a) = (a, [], [])
          go ("class", a) = ("", T.split (' ' ==) a, [])
          go x = ("", [], [x])

-- TODO: can these instances be derived via automatically?
instance Monad m => OndimNode (PandocTag m) ExpansibleText where
  type ExpTypes ExpansibleText = '[]

instance Monad m => OndimNode (PandocTag m) Attribute where
  type ExpTypes Attribute = '[ExpansibleText]
  identify (t,_) = Just t

instance HasSub (PandocTag m) Attribute ExpansibleText where
  getSubs (_,t) = [t]
  setSubs (k,_) (t:_) = (k, t)
  setSubs x _ = x

bindDefaults :: forall m t. Monad m =>
  Ondim (PandocTag m) t -> Ondim (PandocTag m) t
bindDefaults st = st
 `binding` do
   "if-bound" ## ifBound @Block
   "switch" ## switchBound
   "bind" ## bind
   "bind-text" ## bindText stringify
 `binding` do
   "if-bound" ## ifBound @Inline
   "switch" ## switchBound
   "bind" ## bind
   "bind-text" ## bindText stringify
  `bindingFilters` do
    "attrSub" ## attrSub

-- Template loading helpers

blockFromDocument :: Monad m => Pandoc -> Expansion (PandocTag m) Block
blockFromDocument (Pandoc _ b) = fromTemplate b

inlineFromDocument :: Monad m => Pandoc -> Expansion (PandocTag m) Inline
inlineFromDocument (Pandoc _ (Para i : _)) = fromTemplate i
inlineFromDocument _ = ignore

-- Miscellaneous (from Text.Pandoc.Shared)

stringify :: Walkable Inline a => a -> T.Text
stringify = query go
  where go :: Inline -> T.Text
        go Space      = " "
        go SoftBreak  = " "
        go (Str x)    = x
        go (Code _ x) = x
        go (Math _ x) = x
        go LineBreak  = " "
        go _          = ""
