{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parser
  ) where

import Control.Applicative ((<|>), empty)
import Control.Applicative.Permutations
  ( runPermutation
  , toPermutationWithDefault
  )
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec
  , (<?>)
  , between
  , eof
  , many
  , optional
  , parse
  , sepBy
  , sepEndBy
  , some
  , takeWhile1P
  )
import qualified Text.Megaparsec.Char as C (char, eol, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (space, symbol)
import Text.Megaparsec.Error (ParseErrorBundle)
import Types
  ( Block(..)
  , Comment
  , Direction(..)
  , Document(..)
  , IsEdge
  , IsNode
  , Link
  , Name
  , Tag
  )

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

--
-- Lexer
--
spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 empty empty

space :: Parser ()
space = void $ C.char ' '

symbol :: String -> Parser String
symbol = L.symbol (L.space space empty empty)

comma :: Parser String
comma = symbol ","

string :: Parser String
string = takeWhile1P Nothing (not . \c -> c == '\n' || c == '\r')

commaFreeString :: Parser String
commaFreeString =
  takeWhile1P Nothing (not . \c -> c == ',' || c == '\n' || c == '\r')

spaceFreeString :: Parser String
spaceFreeString =
  takeWhile1P Nothing (not . \c -> c == ' ' || c == '\n' || c == '\r')

bracketFreeString :: Parser String
bracketFreeString =
  takeWhile1P Nothing (not . \c -> c == ']' || c == '\n' || c == '\r')

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser' = parser' `sepBy` comma

label :: Parser String
label = (symbol "[" *> bracketFreeString <* "]") <|> spaceFreeString

--
-- Parser
--
parseName :: Parser Name
parseName = C.string "= " *> string <* C.eol

parseTags :: Parser [Tag]
parseTags = C.string "# " *> commaSeparated commaFreeString <* C.eol

parseLink :: Parser Link
parseLink = do
  void $ C.string "/ "
  url <- spaceFreeString
  text <- optional (space *> string)
  void C.eol
  return (url, text)

parseComment :: Parser (Maybe Comment)
parseComment =
  fmap (trimRight . unlines . map trimRight) <$>
  (optional . some) (commentLine <|> emptyLine)
  where
    commentLine = C.string "' " *> string <* C.eol
    emptyLine = C.string "'" *> C.eol

parseExtra :: Parser ([Tag], [Link], Maybe Comment)
parseExtra =
  runPermutation $
  (,,) <$> toPermutationWithDefault [] (parseTags <?> "tags") <*>
  toPermutationWithDefault [] (some (parseLink <?> "link")) <*>
  toPermutationWithDefault Nothing (parseComment <?> "comment")

parseEdge :: Parser (Block IsEdge)
parseEdge = do
  d <- symbol "--" <|> symbol "<>" <|> symbol "<-" <|> symbol "->"
  a <- label
  void space
  b <- label
  comment' <- C.eol *> parseComment
  pure $
    case d of
      "--" -> Edge Undirected a b comment'
      "<>" -> Edge Bidirected a b comment'
      "<-" -> Edge Directed b a comment'
      "->" -> Edge Directed a b comment'
      _ -> error "Not going to happen"

parseNode :: Parser (Block IsNode)
parseNode = do
  name' <- parseName <?> "node-name"
  (tags', links', comment') <- parseExtra
  pure $ Node name' comment' tags' links'

parseDocument :: Parser Document
parseDocument = do
  nodes <- parseNode `sepEndBy` many C.eol
  edges <- parseEdge `sepBy` many C.eol
  pure $ Document nodes edges

parser :: String -> Either ParseError Document
parser = parse (between spaceConsumer eof parseDocument) ""

--
-- Helpers
--
trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse
