{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parser
  ) where

import Control.Applicative ((<|>), empty)
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
import qualified Types as T (Block(..))
import Types
  ( Columns
  , Comment
  , Direction(..)
  , Document(..)
  , IsEdge
  , IsNode
  , Label
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

string :: Parser String
string = takeWhile1P Nothing (not . \c -> c == '\n' || c == '\r')

spaceFreeString :: Parser String
spaceFreeString =
  takeWhile1P Nothing (not . \c -> c == ' ' || c == '\n' || c == '\r')

bracketFreeString :: Parser String
bracketFreeString =
  takeWhile1P Nothing (not . \c -> c == ']' || c == '\n' || c == '\r')

spaceSeparated :: Parser a -> Parser [a]
spaceSeparated p = p `sepBy` space

label :: Parser String
label = (symbol "[" *> bracketFreeString <* "]") <|> spaceFreeString

--
-- Parser
--
parseLabel :: Parser Label -- TODO parseLabel
parseLabel = C.string "= " *> string <* C.eol

parseColumns :: Parser Columns -- TODO parseList `, a b c` `, [has spaces] no-spaces [more spaces]`
parseColumns = C.string "| " *> spaceSeparated label <* C.eol

parseRows :: Parser [Columns]
parseRows = many parseColumns

parseComment :: Parser (Maybe Comment) -- TODO `parseComment`
parseComment =
  fmap (trimRight . unlines . map trimRight) <$>
  (optional . some) (commentLine <|> emptyLine)
  where
    commentLine = C.string "' " *> string <* C.eol
    emptyLine = C.string "'" *> C.eol

parseEdge :: Parser (T.Block IsEdge)
parseEdge = do
  d <- symbol "--" <|> symbol "<>" <|> symbol "<-" <|> symbol "->"
  a <- label
  void space
  b <- label
  comment' <- C.eol *> parseComment
  pure $
    case d of
      "--" -> T.Edge Undirected a b comment'
      "<>" -> T.Edge Bidirected a b comment'
      "<-" -> T.Edge Directed b a comment'
      "->" -> T.Edge Directed a b comment'
      _ -> error "Not going to happen"

parseNode :: Parser (T.Block IsNode)
parseNode = do
  label' <- parseLabel <?> "node-label"
  comment' <- parseComment <?> "comment"
  rows' <- parseRows <?> "rows"
  pure $ T.Node label' comment' rows'

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
