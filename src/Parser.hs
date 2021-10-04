{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Parser
  ( parser
  ) where

import Text.Megaparsec.Debug (dbg)
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
import qualified Text.Megaparsec.Char.Lexer as L
  ( IndentOpt(..)
  , indentBlock
  , lexeme
  , nonIndented
  , skipLineComment
  , space
  , symbol
  )
import Text.Megaparsec.Error (ParseErrorBundle)
import qualified Types as T (Block(..))
import Types
  ( Cells
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
spaceConsumerNewline :: Parser ()
spaceConsumerNewline = L.space C.space1 lineComment empty

spaceConsumer :: Parser ()
spaceConsumer =
  L.space (void $ some (C.char ' ' <|> C.char '\t')) lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment ";" -- FIXME

space :: Parser ()
space = void $ C.char ' '

symbol :: String -> Parser String
symbol = L.symbol (L.space space lineComment empty)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

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

-- TODO: [[foo]] should return [foo]
--
-- Parser
--
pLabel :: Parser Label
pLabel = lexeme $ C.string "= " *> string <* C.eol

pCells :: Parser Cells -- TODO pList `, a b c` `, [has spaces] no-spaces [more spaces]`
pCells = lexeme $ C.string "| " *> spaceSeparated label <* C.eol

pRows :: Parser [Cells]
pRows = many pCells

pComment :: Parser (Maybe Comment)
pComment =
  fmap (trimRight . unlines . map trimRight) <$>
  (optional . some) (commentLine <|> emptyLine)
  where
    commentLine = lexeme $ C.string "' " *> string <* C.eol
    emptyLine = lexeme $ C.string "'" *> C.eol

pEdge :: Parser (T.Block IsEdge)
pEdge = do
  d <- symbol "--" <|> symbol "<>" <|> symbol "<-" <|> symbol "->"
  a <- label
  void space
  b <- label
  comment' <- C.eol *> pComment
  pure $
    case d of
      "--" -> T.Edge Undirected a b comment'
      "<>" -> T.Edge Bidirected a b comment'
      "<-" -> T.Edge Directed b a comment'
      "->" -> T.Edge Directed a b comment'
      _ -> error "Not going to happen"

pNode :: Parser (T.Block IsNode)
pNode = do
  label' <- pLabel <?> "node-label"
  comment' <- pComment <?> "comment"
  rows' <- pRows <?> "rows"
  pure $ T.Node label' comment' rows'

pClusterLabel :: Parser Label
pClusterLabel = lexeme $ C.string ": " *> string -- <* C.eol

pCluster :: Parser (Label, [Document]) -- header and list items
pCluster =
  dbg "cluster" $ L.nonIndented spaceConsumerNewline (L.indentBlock spaceConsumerNewline p)
  where
    p = do
      label <- dbg "cluster-label" pClusterLabel <?> "cluster-label"
      pure (L.IndentSome Nothing (return . (label, )) (dbg "document" $ pDocument label))

-- pDocument :: Parser Document
pDocument clusterLabel = do
  nodes <- dbg "nodes" (pNode `sepEndBy` many C.eol)
  edges <- dbg "edges" (pEdge `sepBy` many C.eol)
  pure $ Document (setClusterNodes clusterLabel nodes) (setClusterEdges clusterLabel edges)

parser :: String -> Either ParseError (Label, [Document]) -- Cluster = (Label, Document)
parser = parse (between spaceConsumerNewline eof pCluster) ""

setClusterNodes :: Label -> [T.Block IsNode] -> [T.Block IsNode]
setClusterNodes = map . setClusterNode

setClusterNode :: Label -> T.Block IsNode -> T.Block IsNode
setClusterNode l n@(T.Node label' _ _) = n {T.label = l ++ ":" ++ label'}

setClusterEdges :: Label -> [T.Block IsEdge] -> [T.Block IsEdge]
setClusterEdges l xs = map (\(T.Edge dir a b comment) -> (T.Edge dir (l ++ ":" ++ a) (l ++ ":" ++ b) comment)) xs

--
-- Helpers
--
trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse
