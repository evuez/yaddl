{-# LANGUAGE OverloadedStrings #-}

import Parser (parser)
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Types (Block(..), Document(..))

main :: IO ()
main =
  hspec $ do
    describe "nodes parser" $ do
      it "parses a well-formed node" $
        parser
          "= Node name\n\
          \' Comment\n\
          \| a b\n\
          \| b a\n\n" `shouldParse`
        (Document
           [Node "Node name" (Just "Comment") [["a", "b"], ["b", "a"]]]
           [])
      it "parses a well-formed node 2" $
        parser
          "= Node name\n\
          \' Comment\n\
          \| a b\n\
          \| b a\n\n\
          \= Node2\n\n\
          \-- Node Node2\n\
          \' label\n" `shouldParse`
        (Document
           [Node "Node name" (Just "Comment") [["a", "b"], ["b", "a"]]]
           [])
