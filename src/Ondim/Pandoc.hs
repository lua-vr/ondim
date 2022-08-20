{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Just because Pandoc.Meta has no IsList instance.
module Ondim.Pandoc where
import Ondim
import Ondim.Extra
import Text.Pandoc.Definition
import qualified Data.Text as T
import Data.HList.ContainsType
import Data.Map.Syntax ((##))
import Text.Pandoc.Walk
import qualified Text.Pandoc.Builder as B

data PandocTag (m :: Type -> Type)

getId :: [Text] -> Maybe Text
getId = asum . map (T.stripPrefix "e:")

newtype Target' = Target' (Text, Text)

instance Monad m => OndimTag (PandocTag m) where
  type OndimTypes (PandocTag m) =
    '[ Pandoc
     , MetaValue
     , (Text, MetaValue)
     , Inline
     , [Inline]
     , Block
     , [Block]
     , ([Inline], [[Block]])
     , Target'
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

instance Monad m => OndimNode (PandocTag m) Pandoc where
  type ExpTypes Pandoc ='[(Text, MetaValue), Block]

instance HasSub (PandocTag m) Pandoc Block
deriving via (Map Text MetaValue) instance IsList Meta
deriving via (PairSub Meta Text MetaValue) instance HasSub (PandocTag m) Pandoc (Text, MetaValue)

instance Monad m => OndimNode (PandocTag m) (Text, MetaValue) where
  type ExpTypes (Text, MetaValue) ='[ExpansibleText, MetaValue]
  identify = Just . fst

deriving via (OneSub Text) instance HasSub (PandocTag m) (Text, MetaValue) ExpansibleText
instance HasSub (PandocTag m) (Text, MetaValue) MetaValue where
  getSubs = one . snd
  setSubs x y = second (\z -> fromMaybe z (viaNonEmpty head y)) x -- ugh :c

instance Monad m => OndimNode (PandocTag m) MetaValue where
  type ExpTypes MetaValue ='[(Text, MetaValue), MetaValue, ExpansibleText, Inline, Block]
  fromText = Just (one . MetaString)

instance HasSub (PandocTag m) MetaValue MetaValue
instance HasSub (PandocTag m) MetaValue Inline
instance HasSub (PandocTag m) MetaValue Block
deriving via (MapSub Text MetaValue) instance HasSub (PandocTag m) MetaValue (Text, MetaValue)
deriving via (OneSub Text) instance HasSub (PandocTag m) MetaValue ExpansibleText

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
  identify (Div (_,n,_) _) = getId n
  identify (Header _ (_,n,_) _) = getId n
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
  type ExpTypes Inline = '[Inline, Block, Attribute, Target', ExpansibleText]
  identify (Span (_,n,_) _) = getId n
  identify _ = Nothing
  fromText = Just (toList . B.text)
  validIdentifiers = Just []

instance HasSub (PandocTag m) Inline Inline
instance HasSub (PandocTag m) Inline Block
deriving via (NestedSub Attr Attribute) instance HasSub (PandocTag m) Inline Attribute
deriving via (OneSub ExpansibleText) instance HasSub (PandocTag m) Inline ExpansibleText
instance HasSub (PandocTag m) Inline Target' where
  getSubs (Link _ _ t) = [Target' t]
  getSubs (Image _ _ t) = [Target' t]
  getSubs _ = []
  setSubs (Link x y _) [z] = Link x y (coerce z)
  setSubs (Image x y _) [z] = Image x y (coerce z)
  setSubs x _ = x

instance Monad m => OndimNode (PandocTag m) Target' where
  type ExpTypes Target' = '[Text]

instance HasSub (PandocTag m) Target' Text where
  getSubs (Target' (s,t)) = [s,t]
  setSubs _ [a,b] = Target' (a,b)
  setSubs x _ = x

instance Monad m => OndimNode (PandocTag m) ([Inline], [[Block]]) where
  type ExpTypes ([Inline], [[Block]]) = '[Inline, [Block]]
  identify (_, (x : _)) = identify @(PandocTag m) x
  identify _ = Nothing

instance HasSub (PandocTag m) ([Inline], [[Block]]) Inline
instance HasSub (PandocTag m) ([Inline], [[Block]]) [Block]

instance HasSub (PandocTag m) Attr Attribute where
  getSubs (x,y,z) =
    ("id", x) :
    ("class", T.intercalate " " (filter (not . T.isPrefixOf "e:") y)) :
    z
  setSubs _ = foldMap go
    where go ("id", a) = (a, [], [])
          go ("class", a) = ("", T.split (' ' ==) a, [])
          go x = ("", [], [x])

-- TODO: can these instances be derived via automatically?
instance Monad m => OndimNode (PandocTag m) ExpansibleText where
  type ExpTypes ExpansibleText = '[]

instance Monad m => OndimNode (PandocTag m) Attribute where
  type ExpTypes Attribute = '[ExpansibleText]
  identify = Just . fst

instance HasSub (PandocTag m) Attribute ExpansibleText where
  getSubs (_,t) = [t]
  setSubs (k,_) t = (k, mconcat t)

bindDefaults :: forall m t. Monad m =>
  Ondim (PandocTag m) t -> Ondim (PandocTag m) t
bindDefaults st = st
 `binding` do
   "if" ## ifBound @Block
   "switch" ## switchBound
   "bind" ## bind
   "scope" ## scope
   "bind-text" ## bindText stringify
 `binding` do
   "if" ## ifBound @Inline
   "switch" ## switchBound
   "bind" ## bind
   "scope" ## scope
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
