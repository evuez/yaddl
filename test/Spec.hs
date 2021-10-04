{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (encode)
import Lib (semCheck)
import Parser (parser)
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Types (Block(..), Direction(..), Document(..))

main :: IO ()
main =
  hspec $ do
    describe "nodes parser" $ do
      it "parses a well-formed node" $
        parser
          "= Node name\n\
          \# Tag 1, Tag 2\n\
          \/ https://example.com/\n\
          \/ https://example.com/ An example\n\
          \' Comment\n" `shouldParse`
        Document
          [ Node
              "Node name"
              (Just "Comment")
              ["Tag 1", "Tag 2"]
              [ ("https://example.com/", Nothing)
              , ("https://example.com/", Just "An example")
              ]
          ]
          []
      it "parses a well-formed node with no link" $
        parser
          "= Node name\n\
          \# Tag 1, Tag 2\n\
          \' Comment\n" `shouldParse`
        Document [Node "Node name" (Just "Comment") ["Tag 1", "Tag 2"] []] []
      it "parses a well-formed node with no link and no tag" $
        parser
          "= Node name\n\
          \' Comment\n" `shouldParse`
        Document [Node "Node name" (Just "Comment") [] []] []
      it "parses a well-formed node with no link, no tag and no comment" $
        parser "= Node name\n" `shouldParse`
        Document [Node "Node name" Nothing [] []] []
      it "parses a well-formed node with a multiline comment" $
        parser
          "= Node name\n\
               \' Line 1\n\
               \' Line 2\n" `shouldParse`
        Document [Node "Node name" (Just "Line 1\nLine 2") [] []] []
      it "ignores fields ordering" $
        parser
          "= Node name\n\
          \' Comment\n\
          \/ https://example.com/\n\
          \/ https://example.com/ An example\n\
          \# Tag 1, Tag 2\n" `shouldParse`
        Document
          [ Node
              "Node name"
              (Just "Comment")
              ["Tag 1", "Tag 2"]
              [ ("https://example.com/", Nothing)
              , ("https://example.com/", Just "An example")
              ]
          ]
          []
      it "parses a list of nodes" $
        parser
          "= Node 1\n\
          \' Comment 1\n\n\
          \= Node 2\n" `shouldParse`
        Document
          [Node "Node 1" (Just "Comment 1") [] [], Node "Node 2" Nothing [] []]
          []
    describe "edges parser" $ do
      it "parses an undirected edge" $
        parser "-- node-a node-b\n" `shouldParse`
        Document [] [Edge Undirected "node-a" "node-b" Nothing]
      it "parses an bidirected edge" $
        parser "<> node-a node-b\n" `shouldParse`
        Document [] [Edge Bidirected "node-a" "node-b" Nothing]
      it "parses a left-to-right edge" $
        parser "-> node-a node-b\n" `shouldParse`
        Document [] [Edge Directed "node-a" "node-b" Nothing]
      it "parses a right-to-left edge" $
        parser "<- node-a node-b\n" `shouldParse`
        Document [] [Edge Directed "node-b" "node-a" Nothing]
      it "parses an edge with a comment" $
        parser
          "-- node-a node-b\n\
          \' Comment 1\n" `shouldParse`
        Document [] [Edge Undirected "node-a" "node-b" (Just "Comment 1")]
      it "parses an edge with a multiline comment" $
        parser "-- node-a [node b]\n" `shouldParse`
        Document [] [Edge Undirected "node-a" "node b" Nothing]
      it "parses an edge with spaces in one of the node name" $
        parser
          "-- node-a node-b\n\
          \' Comment 1\n\
          \' Comment 2\n" `shouldParse`
        Document
          []
          [Edge Undirected "node-a" "node-b" (Just "Comment 1\nComment 2")]
    describe "parser" $ do
      it "parses a full yaddl document" $
        parser
          "= node-a\n\
          \\n\
          \= node-b\n\
          \# Tag 1\n\
          \/ https://example.com Example\n\
          \' Node b, 1\n\
          \'\n\
          \' Node b, 2\n\
          \= node-c\n\
          \' Node c\n\
          \\n\
          \-- node-a node-b\n\
          \' Comment 1\n\
          \' Comment 2\n" `shouldParse`
        Document
          [ Node "node-a" Nothing [] []
          , Node
              "node-b"
              (Just "Node b, 1\n\nNode b, 2")
              ["Tag 1"]
              [("https://example.com", Just "Example")]
          , Node "node-c" (Just "Node c") [] []
          ]
          [Edge Undirected "node-a" "node-b" (Just "Comment 1\nComment 2")]
    describe "semCheck" $ do
      it "ensures nodes are unique" $
        semCheck
          (Document
             [Node "node-a" Nothing [] [], Node "node-a" Nothing [] []]
             []) `shouldBe`
        (Left "Found duplicate nodes: node-a")
      it "ensures edges are unique" $
        semCheck
          (Document
             [Node "node-a" Nothing [] [], Node "node-b" Nothing [] []]
             [ Edge Undirected "node-a" "node-b" Nothing
             , Edge Undirected "node-a" "node-b" Nothing
             ]) `shouldBe`
        (Left "Found duplicate edges: node-a -- node-b")
      it "ensures edges bind existing nodes" $
        semCheck
          (Document
             [Node "node-a" Nothing [] []]
             [Edge Undirected "node-a" "node-b" Nothing]) `shouldBe`
        (Left "In edge node-a -- node-b: node-b does not exist")
    describe "encoder" $ do
      it "encodes a card" $
        encode
          (Node
             "Name 1"
             (Just "A")
             ["1", "2"]
             [("https://example.org", Nothing)]) `shouldBe`
        "{\"name\":\"Name 1\",\"links\":[[\"https://example.org\",null]],\"comment\":\"A\",\"tags\":[\"1\",\"2\"]}"
      it "encodes a list of cards" $
        encode [Node "Name 1" (Just "A") ["1", "2"] []] `shouldBe`
        "[{\"name\":\"Name 1\",\"links\":[],\"comment\":\"A\",\"tags\":[\"1\",\"2\"]}]"
