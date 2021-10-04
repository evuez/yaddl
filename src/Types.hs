{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types
  ( Direction(..)
  , Block(..)
  , Document(..)
  , IsNode
  , IsEdge
  , Label
  , Comment
  , Cells
  , Error
  ) where

import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as A (object)
import Data.Maybe (fromMaybe)

type Error = String

type Label = String

type Comment = String

type Cells = [String]

data Direction
  = Directed
  | Bidirected
  | Undirected
  deriving (Show, Eq, Ord)

data IsNode

data IsEdge

data Block a where -- TODO: Make cluster a part of the block instead of just a string in front of the name
  Edge :: Direction -> Label -> Label -> Maybe Comment -> Block IsEdge
  Node
    :: { label :: Label
       , comment :: Maybe Comment
       , rows :: [Cells]}
    -> Block IsNode

deriving instance Ord (Block a)

deriving instance Eq (Block a)

data Document =
  Document [Block IsNode]
           [Block IsEdge]
  deriving (Show, Eq)

--data Document = Document { clusters :: [Cluster], nodes :: [Node], edges :: [Edge] }
data Cluster = Cluster (Maybe Label) [Node] [Edge]

--
-- Classes
--
--class HasNode a where
--  setCluster :: Label -> a -> a

--
-- Instances
--
-- instance HasNode (Block a) where -- TODO: Lens
--   setCluster clusterLabel (Edge dir a b comment) = Edge dir (setCluster clusterLabel a) (setCluster clusterLabel b) comment
--   setCluster clusterLabel n@(Node label' _ _) = n { label = (label' ++ ":" ++ clusterLabel) }

-- instance Eq (Block a) where
--   Edge d1 a1 b1 _ == Edge d2 a2 b2 _ = d1 == d2 && a1 == a2 && b1 == b2
--   Node n1 _ _ == Node n2 _ _ = n1 == n2
instance Show (Block a) where
  show (Edge Directed a b _) = a ++ " -> " ++ b
  show (Edge Undirected a b _) = a ++ " -- " ++ b
  show (Edge Bidirected a b _) = a ++ " <> " ++ b
  show (Node n c r) = n ++ "[" ++ (fromMaybe "" c) ++ "]" ++ (mconcat . mconcat) r

instance ToJSON Document where
  toJSON (Document nodes edges) =
    A.object ["nodes" .= toJSON nodes, "edges" .= toJSON edges]

instance ToJSON (Block a) where
  toJSON node@Node {} =
    A.object
      ["label" .= label node, "comment" .= comment node, "rows" .= rows node]
  toJSON (Edge direction nodeA nodeB comment') =
    A.object
      [ "direction" .= show direction
      , "node_a" .= nodeA
      , "node_b" .= nodeB
      , "comment" .= comment'
      ]
