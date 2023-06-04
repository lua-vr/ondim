module Ondim.Targets.Whiskers.Parser (parseWhiskers) where

import Data.Char (isAlphaNum, isSpace, isSymbol)
import Data.Sequence (Seq (..), (|>))
import Data.Text qualified as T
import Ondim.Targets.Whiskers.Instances (Node (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (many, some)

newtype Nodes = Nodes {unNodes :: Seq Node}

instance Semigroup Nodes where
  (Nodes (xs :|> Textual t1)) <> (Nodes (Textual t2 :<| ys)) =
    Nodes ((xs |> Textual (t1 <> t2)) <> ys)
  (Nodes xs) <> (Nodes ys) =
    Nodes (xs <> ys)

instance Monoid Nodes where
  mempty = Nodes mempty

instance One Nodes where
  type OneItem Nodes = Node
  one = Nodes . one

toNodeList :: Nodes -> [Node]
toNodeList = toList . unNodes

data ParserState = ParserState
  { delimiters :: (Text, Text),
    whitespace :: Text,
    bolspace :: Text,
    alone :: Bool,
    level :: Int
  }

initialParserstate :: ParserState
initialParserstate = ParserState ("<<", ">>") "" "" True 0

type Parser = StateT ParserState (Parsec Void Text)

parseBool :: Parser a -> Parser Bool
parseBool m = option False $ m $> True

hwhitespace :: Parser Text
hwhitespace = takeWhileP Nothing (\c -> isSpace c && c /= '\n')

hwhitespace1 :: Parser Text
hwhitespace1 = takeWhile1P Nothing (\c -> isSpace c && c /= '\n')

bolspace' :: Parser ()
bolspace' = do
  ws <- hwhitespace
  modify \s -> s {bolspace = ws, alone = True}

trimmedBOL :: Parser Text
trimmedBOL = gets \s -> T.drop (2 * level s) (bolspace s)

newline' :: Parser ()
newline' = do
  _ <- newline
  bol <- trimmedBOL
  modify \s -> s {whitespace = whitespace s <> "\n" <> bol}
  bolspace'

openDelimiter :: Parser (Text, Text)
openDelimiter = do
  _ <- string =<< gets (fst . delimiters)
  (ws, bolws) <- (char '~' $> mempty) <|> liftA2 (,) (gets whitespace) trimmedBOL
  modify \s -> s {whitespace = "", bolspace = ""}
  return (ws, bolws)

closeDelimiter :: Parser ()
closeDelimiter = do
  stripEnd <- parseBool $ char '~'
  _ <- string =<< gets (snd . delimiters)
  whenM (gets alone) do
    void $ optional $ try $ hspace *> newline *> bolspace'
  when stripEnd do
    _ <- many newline'
    modify \s -> s {whitespace = "", bolspace = ""}
  modify \s -> s {alone = False}

space' :: Parser ()
space' = do
  ws <- hwhitespace1
  modify \s -> s {whitespace = whitespace s <> ws}

parseWhiskers :: (Text, Text) -> String -> Text -> Either String [Node]
parseWhiskers d fp =
  first errorBundlePretty
    . parse (evalStateT document initialParserstate {delimiters = d}) fp

document :: Parser [Node]
document = do
  bolspace'
  Nodes nodes <- manyNodes
  ws <- liftA2 (<>) (gets whitespace) (gets bolspace)
  eof
  return $ toList $ nodes |> Textual ws

manyNodes :: Parser Nodes
manyNodes =
  mconcat
    <$> many
      ( rawText
          <|> space' $> mempty
          <|> newline' $> mempty
          <|> sectionStart
      )

rawText :: Parser Nodes
rawText = do
  txt <-
    mconcat <$> some do
      notFollowedBy openDelimiter
      liftA2 T.cons (satisfy (not . isSpace)) $
        takeWhileP Nothing (\c -> not (isSymbol c || isSpace c))
  ws <- liftA2 (<>) (gets whitespace) trimmedBOL
  modify \s -> s {whitespace = "", bolspace = "", alone = False}
  return $ one $ Textual (ws <> txt)

sectionStart :: Parser Nodes
sectionStart = do
  (ws, bolws) <- try $ openDelimiter <* notFollowedBy (char '/')
  (alone', level') <- gets (alone &&& level)
  space
  isSection <- parseBool $ char '#'
  name <- takeWhileP (Just "node name") isAllowedName
  attributes <- option [] $ space1 *> (pair `sepBy1'` space1)
  space
  closeDelimiter
  if isSection
    then do
      modify \s -> s {level = level s + 1}
      child <- manyNodes
      modify \s -> s {alone = alone', level = level'}
      end <- sectionEnd name <?> "end of section " <> show name
      return $
        Nodes
          ( Empty
              |> Textual ws
              |> Section name attributes (toNodeList $ child <> end)
          )
    else
      return $
        Nodes
          ( Empty
              |> Textual (ws <> bolws)
              |> Single name attributes
          )
  where
    sepBy1' p sep = (:) <$> p <*> many (try $ sep >> p)
    str =
      (char '"' *> (toText <$> manyTill L.charLiteral (char '"')))
        <|> takeWhile1P Nothing isAllowedName
    pair = try $ liftA2 (,) str $ option "" $ try $ space *> char '=' *> space *> str

isAllowedName :: Char -> Bool
isAllowedName c =
  isAlphaNum c
    || c == '-'
    || c == '.'
    || c == ':'
    || c == '@'
    || c == '&'
    || c == '%'
    || c == '#'
    || c == '!'

sectionEnd :: Text -> Parser Nodes
sectionEnd name = do
  (ws, _) <- openDelimiter
  hspace
  _ <- char '/'
  _ <- optional (string name)
  hspace
  closeDelimiter
  return $ one $ Textual ws
