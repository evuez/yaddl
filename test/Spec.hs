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
          --": cluster\n\
          ": cluster\n\
          \  = Node name\n\
          \  ' Comment\n\
          \  | a b\n\
          \  | [b c] a\n\
          \\
          \  = Bar\n\
          \  ' Comment\n\
          \  | a b\n\
          \  | [b c] a\n\
          \\
          \  -- [Node name] Bar\n\
          \\
          \: cluster2\n\
          \  = Node nae\n\
          \  ' Comment\n\
          \  | a b\n\
          \  | [b c] a\n\
          \\
          \  = Br\n\
          \  ' Comment\n\
          \  | a b\n\
          \  | [b c] a\n\
          \\
          \ " `shouldParse`
        ( ""
        , [ Document
              [Node "Node name" (Just "Comment") [["a", "b"], ["b", "a"]]]
              []
          ])

