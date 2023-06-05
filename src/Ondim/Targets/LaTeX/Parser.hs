-- | This is a fake LaTeX parser.
module Ondim.Targets.LaTeX.Parser where

import Data.Char (isAsciiLower, isAsciiUpper, isSpace, isUpper, toLower, isSymbol)
import Data.Sequence (Seq (..), (|>))
import Data.Text qualified as T
import Ondim.Targets.LaTeX.Instances (Node (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, some)

parseLaTeX :: String -> Text -> Either String [Node]
parseLaTeX fp =
  first errorBundlePretty
    . parse (runReaderT document initialParserstate) fp

document :: Parser [Node]
document = do
  nodes <- manyNodes
  eof
  return $ toNodeList nodes

newtype Nodes = Nodes {unNodes :: Seq Node}

instance Semigroup Nodes where
  (Nodes (xs :|> Text t1)) <> (Nodes (Text t2 :<| ys)) =
    Nodes ((xs |> Text (t1 <> t2)) <> ys)
  (Nodes xs) <> (Nodes ys) =
    Nodes (xs <> ys)

instance Monoid Nodes where
  mempty = Nodes mempty

instance One Nodes where
  type OneItem Nodes = Node
  one = Nodes . one

toNodeList :: Nodes -> [Node]
toNodeList = toList . unNodes

newtype ParserState = ParserState
  { level :: Int
  }

initialParserstate :: ParserState
initialParserstate = ParserState 0

type Parser = ReaderT ParserState (Parsec Void Text)

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n'

manyNodes :: Parser Nodes
manyNodes =
  mconcat
    <$> many
      ( braces
          <|> lineEnd
          <|> command
          <|> escape
          <|> spaceEater
          <|> comment
          <|> prose
      )

command :: Parser Nodes
command = do
  _ <- try $ string "\\@"
  name <- takeWhileP Nothing isAllowedName
  attrs <- option [] $ try do
    space
    _ <- char '['
    space
    pair `sepBy` char ',' <* char ']'
  arg <- option [] $ try do
    space
    _ <- char '{'
    inner <* char '}'
  return $ one (Command (unCamel name) attrs arg)
  where
    unCamel = T.concatMap \c ->
      if isUpper c
        then T.pack ['-', toLower c]
        else one c
    inner =
      local
        (\s -> s {level = 1 + level s})
        (toNodeList <$> manyNodes)
    isAllowedKey c = isAllowedName c || c == '.' || c == '-' || c == ':'
    gpVal = char '{' *> (toNodeList <$> manyNodes) <* char '}'
    nVal = one . Text <$> takeWhile1P Nothing isAllowedKey
    pair = do
      space
      k <- try gpVal <|> nVal
      space
      v <- option [] $ do
        char '=' *> space
        (try gpVal <|> nVal)
          <* space
      return (k, v)

lineEnd :: Parser Nodes
lineEnd = do
  _ <- char '\n'
  n <- asks level
  s <- takeWhileP Nothing isHSpace
  return $ one $ Text $ T.cons '\n' $ T.drop (2 * n) s

escape :: Parser Nodes
escape = try do
  _ <- char '\\'
  s <- satisfy isSymbol
  return $ one $ Text $ T.pack ['\\', s]

prose :: Parser Nodes
prose = do
  s <- satisfy \c -> c /= '\n' && c /= '}' && c /= '%'
  t <-
    takeWhileP
      Nothing
      (\c -> c /= '\\' && c /= '\n' && c /= '{' && c /= '}' && c /= '%')
  return $ Nodes (one $ Text $ T.cons s t)

comment :: Parser Nodes
comment = do
  _ <- try $ char '%'
  c <- takeWhileP Nothing (/= '\n')
  return $ Nodes (one $ Comment c)

braces :: Parser Nodes
braces = do
  _ <- try $ char '{'
  x <- manyNodes
  _ <- char '}'
  return $ one (Text "{") <> x <> one (Text "}")

spaceEater :: Parser Nodes
spaceEater = do
  _ <- try $ string "%!\n"
  space
  return mempty

isAllowedName :: Char -> Bool
isAllowedName c = isAsciiLower c || isAsciiUpper c || c == '@'
