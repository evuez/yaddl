{-# LANGUAGE GADTs #-}

module Lib
  ( mkDocument
  , semCheck
  ) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Set as S (elems, empty, insert)
import Parser (parser)
import Text.Megaparsec.Error (errorBundlePretty)
import Types (Block(..), Document(..), Error, IsEdge, IsNode)

mkDocument :: String -> Either Error Document
mkDocument s = do
  document <- first errorBundlePretty (parser s)
  void $ semCheck document
  pure document

--
-- Semantic checks
--
semCheck :: Document -> Either Error ()
semCheck (Document nodes edges) = do
  void $ uniqueBlocks "nodes" nodes
  void $ uniqueBlocks "edges" edges
  void $ validEdges nodes edges

uniqueBlocks :: String -> [Block a] -> Either Error ()
uniqueBlocks kind xs =
  if null dups'
    then Right ()
    else Left ("Found duplicate " ++ kind ++ ": " ++ showDups dups')
  where
    dups' = dups xs
    showDups = intercalate ", " . map show

validEdges :: [Block IsNode] -> [Block IsEdge] -> Either Error ()
validEdges nodes edges = mconcat <$> mapM (validEdge nodes) edges

validEdge :: [Block IsNode] -> Block IsEdge -> Either Error ()
validEdge xs x@(Edge _ a b _) = mconcat <$> sequence [exists a, exists b]
  where
    labels = map label xs
    exists n =
      if n `elem` labels
        then Right ()
        else Left ("In edge " ++ show x ++ ": " ++ n ++ " does not exist")

--
-- Helpers
--
dups :: Ord a => [a] -> [a]
dups xs = S.elems (go S.empty xs)
  where
    go s (x:xs')
      | x `elem` xs' = go (S.insert x s) xs'
      | otherwise = go s xs'
    go s _ = s
