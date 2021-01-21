{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Writer.Dot
  ( mkDot
  ) where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import Text.Dot (Dot, NodeId, attribute, edge, node)
import Types (Block(..), Direction(..), Document(..), IsEdge, IsNode)

mkDot :: Document -> Dot ()
mkDot (Document nodes edges) = do
  attribute ("rankdir", "LR")
  nodes' <- map2M name dNode nodes
  case mapM (dEdge nodes') edges of
    Just xs -> sequence_ xs
    Nothing -> pure ()

dNode :: Block IsNode -> Dot NodeId
dNode Node {name = name', tags = tags', comment = comment', links = links'} =
  node
    [ ("shape", "Mrecord")
    , ( "label"
      , col
          [ name'
          , intercalate ", " tags'
          , maybeToString comment'
          , row $ map (liftFst maybeToString . swap) links'
          ])
    ]

dEdge :: [(String, NodeId)] -> Block IsEdge -> Maybe (Dot ())
dEdge nodes (Edge direction x y comment') = do
  n1 <- lookup x nodes
  n2 <- lookup y nodes
  pure $ edge n1 n2 attrs
  where
    attrs =
      catMaybes
        [("label", ) <$> comment', Just ("dir", dirtype), ("style", ) <$> style]
    (dirtype, style) =
      case direction of
        Directed -> ("forward", Nothing)
        Bidirected -> ("both", Just "bold")
        Undirected -> ("none", Just "dotted")

col :: [String] -> String
col xs = intercalate "|" (filter (not . null) xs)

row :: [(String, String)] -> String
row xs =
  if null' left || null' right
    then unlines (map (\(x, y) -> mconcat [x, y]) xs)
    else concat ["{", unlines left, "|", unlines right, "}"]
  where
    null' = not . (not . all null)
    (left, right) = unzip xs

maybeToString :: Maybe String -> String
maybeToString (Just s) = s
maybeToString Nothing = ""

map2M :: Monad m => (a -> c) -> (a -> m d) -> [a] -> m [(c, d)]
map2M _ _ [] = pure []
map2M f g (x:xs) = do
  y <- g x
  rs <- map2M f g xs
  pure ((f x, y) : rs)

liftFst :: (a -> b) -> (a, c) -> (b, c)
liftFst f (a, c) = (f a, c)
