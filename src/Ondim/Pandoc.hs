module Ondim.Pandoc where
import Ondim
import Ondim.Extra
import Text.Pandoc.Definition
import qualified Data.Text as T

data PandocTag (m :: Type -> Type)

instance Monad m => OndimTag (PandocTag m) where
  type OndimTypes (PandocTag m) =
    '[ Inline
     , [Inline]
     , Block
     , [Block]
     , ([Inline], [[Block]])
     , Attribute
     , Attr
     , ExpansibleText
     ]
  type OndimMonad (PandocTag m) = m

instance Monad m => OndimNode (PandocTag m) Block where
  type ExpTypes Block =
    '[ Inline
     , [Inline]
     , Block
     , [Block]
     , ([Inline], [[Block]])
     , Attr
     , ExpansibleText
     ]
  identify (Div (T.stripPrefix "e:" -> Just n,_,_) _) = Just n
  identify _ = Nothing
  validIdentifiers = Just []

instance HasSub (PandocTag m) Block Inline
instance HasSub (PandocTag m) Block [Inline]
instance HasSub (PandocTag m) Block Block
instance HasSub (PandocTag m) Block ([Inline], [[Block]])
instance HasSub (PandocTag m) Block [Block]
deriving via (OneSub Attr) instance HasSub (PandocTag m) Block Attr
deriving via (OneSub ExpansibleText) instance HasSub (PandocTag m) Block ExpansibleText

instance Monad m => OndimNode (PandocTag m) Inline where
  type ExpTypes Inline = '[Inline, Block, Attr, ExpansibleText]
  identify (Span (T.stripPrefix "e:" -> Just n,_,_) _) = Just n
  identify _ = Nothing
  fromText = Just Str
  validIdentifiers = Just []

instance HasSub (PandocTag m) Inline Inline
instance HasSub (PandocTag m) Inline Block
deriving via (OneSub Attr) instance HasSub (PandocTag m) Inline Attr
deriving via (OneSub ExpansibleText) instance HasSub (PandocTag m) Inline ExpansibleText

instance Monad m => OndimNode (PandocTag m) ([Inline], [[Block]]) where
  type ExpTypes ([Inline], [[Block]]) = '[Inline, [Block]]
  identify ((Span (n,_,_) _ : _), _) = Just n
  identify _ = Nothing

instance HasSub (PandocTag m) ([Inline], [[Block]]) Inline
instance HasSub (PandocTag m) ([Inline], [[Block]]) [Block]

instance Monad m => OndimNode (PandocTag m) Attr where
  type ExpTypes Attr = '[Attribute]

instance HasSub (PandocTag m) Attr Attribute where
  getSubs (x,y,z) = ("id", x) : ("class", T.intercalate " " y) : z
  setSubs _ = foldMap go
    where go ("id", a) = (a, [], [])
          go ("class", a) = ("", T.split (' ' ==) a, [])
          go x = ("", [], [x])

-- TODO: can these instances be derived via automatically?
instance Monad m => OndimNode (PandocTag m) ExpansibleText where
  type ExpTypes ExpansibleText = '[]
  identify _ = Just ""

instance Monad m => OndimNode (PandocTag m) Attribute where
  type ExpTypes Attribute = '[ExpansibleText]
  identify (t,_) = Just t

instance HasSub (PandocTag m) Attribute ExpansibleText where
  getSubs (_,t) = [t]
  setSubs (k,_) (t:_) = (k, t)
  setSubs x _ = x
