-- | This file is the heart of Ondim. Here we define the more general template
-- expansion using trees.

module Ondim where
import Data.Tree
import Relude.Extra.Map
import Data.Map.Syntax
import Control.Monad.Except

type Expansion m t = OndimT t m (Tree t) -> OndimT t m (Forest t)

type Expansions m t = Map (Identifier t) (Expansion m t)

data OndimS m t = OndimS
  { expansions :: Expansions m t
  , expansionDepth :: Int
  , expansionTrace :: [Text]
  , afterExpansion :: Forest t -> Forest t
  }

emptyOndimS :: (OndimNode t) => OndimS m t
emptyOndimS = OndimS mempty 0 [] id

data OndimException
  = FromTreeGaveNothing
  | MaxExpansionDepthExceeded [Text]
  deriving (Show)

newtype OndimT t m a = OndimT { unOndimT :: ReaderT (OndimS m t) (ExceptT OndimException m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadError OndimException)

instance MonadTrans (OndimT t) where
  lift = OndimT . lift . lift

runOndimT :: OndimT t m a -> OndimS m t -> m (Either OndimException a)
runOndimT o = runExceptT . runReaderT (unOndimT o)

withOdimS ::
  (OndimS m t -> OndimS m t) ->
  OndimT t m a ->
  OndimT t m a
withOdimS f = OndimT . withReaderT f . unOndimT

asksE :: Monad m => (Expansions m t -> a) -> OndimT t m a
asksE f = OndimT $ asks (f . expansions)

class (Ord (Identifier t), IsString (Identifier t), Show (Identifier t)) => OndimNode t where
  type Identifier t
  type Identifier t = Text
  identify :: t -> Maybe (Identifier t)
  children :: Tree t -> Forest t
  children (Node _ c) = c
  attributes :: Tree t -> Map Text Text

-- | This function does most of the magic: it recursively lifts the nodes into
-- an unvaluated state, that can be evaluated with the defined expansions.
liftNodeTree :: (Monad m, OndimNode t) =>
  Tree t -> Either (Text, Forest t) (OndimT t m (Forest t))
liftNodeTree tree@(Node el childs) =
  fromMaybe (Right doNothing) $ do
    name <- identify el
    if name == "bind"
    then do
      tag <- lookup "tag" (attributes tree)
      pure $ Left (tag, properChilds)
    else pure $ Right $
      asksE (lookup name) >>= \case
        Just expansion -> do
          depth <- OndimT $ asks expansionDepth
          eTrace <- OndimT $ asks expansionTrace
          when (depth >= 200) $ -- To avoid recursive expansions
            throwError (MaxExpansionDepthExceeded eTrace)
          withOdimS (\s -> s { expansionDepth = depth + 1
                             , expansionTrace = show name : eTrace}) $
            expansion (Node el <$> liftedChilds)
        Nothing -> doNothing
  where
    properChilds = children tree
    doNothing = one . Node el <$> liftedChilds
    liftedChilds = liftNodeForest childs
  
liftNodeForest :: (Monad m, OndimNode t) =>
  Forest t -> OndimT t m (Forest t)
liftNodeForest f = do
  let sub = map liftNodeTree f
      binds = mapMaybe (fmap toExp . leftToMaybe) sub
      nodes = mapMaybe rightToMaybe sub
  withExpansions (fromList binds) $
    join <$> sequence nodes
  where
    toExp = bimap fromText fromTemplate'

-- | Bind expansions using the more abstract interface to Odim (trees).
-- You generally want to use the more user-friendly `bindingExpansions`.
withExpansions :: OndimNode t => Expansions m t -> OndimT t m a -> OndimT t m a
withExpansions exps (OndimT readr) = OndimT $
  withReaderT (\s -> s {expansions = exps <> expansions s}) readr

type Expansions' m t = MapSyntax (Identifier t) (Expansion m t)

-- | Convenience function to bind using MapSyntax, but still using trees. In
-- most cases it's nicer to user `bindingExpansions` instead (see below).
bindingExpansions' :: OndimNode t =>
  OndimT t m a -> Expansions' m t -> OndimT t m a
bindingExpansions' o exps = withExpansions (fromRight mempty (runMap exps)) o

class OndimNode t => OndimRepr t a | a -> t where
  toTree :: a -> Tree t
  fromTree :: Tree t -> Maybe a

fromReprExpansion :: (Monad m, OndimRepr t a) =>
  (OndimT t m a -> OndimT t m [a]) -> Expansion m t
fromReprExpansion expansion argNode = do
  toTree <<$>> expansion do
    tree <- fromTree <$> argNode
    case tree of
      Just tree' -> pure tree'
      Nothing -> throwError FromTreeGaveNothing
  `catchError` \case
    FromTreeGaveNothing -> one <$> argNode
    x -> throwError x

type ReprExpansions m t a = MapSyntax (Identifier t) (OndimT t m a -> OndimT t m [a])

-- | Bind expansions using MapSyntax.
bindingExpansions :: (Monad m, OndimRepr t a) =>
  OndimT t m b -> ReprExpansions m t a -> OndimT t m b
bindingExpansions o exps = bindingExpansions' o $ mapV fromReprExpansion exps

runChildrenWith ::
  (Monad m, OndimRepr t a) =>
  ReprExpansions m t a  -> Expansion m t
runChildrenWith exps node = (children <$> node) `bindingExpansions` exps

runChildrenWith' ::
  (Monad m, OndimNode t) =>
  Expansions' m t -> Expansion m t
runChildrenWith' exps node = (children <$> node) `bindingExpansions'` exps

fromTemplate' ::
  (Monad m, OndimNode t) =>
  Forest t -> Expansion m t
fromTemplate' tpl inner =
  liftNodeForest tpl `bindingExpansions'` do
    "apply-content" ## const (children <$> inner)

fromTemplate ::
  (Monad m, OndimRepr t a) =>
  [a] -> Expansion m t
fromTemplate = fromTemplate' . fmap toTree
  
class HasEmpty t where
  emptyValue :: t

emptyNode :: HasEmpty t => Tree t
emptyNode = Node emptyValue []
  
callExpansion ::
  (Monad m, OndimNode t, HasEmpty t)
  => (Identifier t) -> OndimT t m (Forest t)
callExpansion name = do
  exps <- asksE (lookup name)
  maybe (pure []) ($ pure emptyNode) exps


-- Convenience

fromText :: IsString a => Text -> a
fromText = fromString . toString
