{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Writer.Dot
  ( mkDot
  ) where

import Data.List (intercalate, maximumBy, transpose)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Text.Dot (Dot, NodeId, attribute, edge, node)
import Types (Columns, Direction(..), Document(..), IsEdge, IsNode)
import qualified Types as T (Block(..))

mkDot :: Document -> Dot ()
mkDot (Document nodes edges) = do
  attribute ("rankdir", "LR")
  nodes' <- map2M T.label dNode nodes
  case mapM (dEdge nodes') edges of
    Just xs -> sequence_ xs
    Nothing -> pure ()

dNode :: T.Block IsNode -> Dot NodeId
dNode T.Node {T.label = label', T.comment = comment', T.rows = rows'} =
  node
    [ fontAttr
    , ("shape", "Mrecord")
    , ("label", col [label', maybeToString comment', rows rows'])
    ]

dEdge :: [(String, NodeId)] -> T.Block IsEdge -> Maybe (Dot ())
dEdge nodes (T.Edge direction x y comment') = do
  n1 <- lookup x nodes
  n2 <- lookup y nodes
  pure $ edge n1 n2 (fontAttr : attrs)
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

rows :: [Columns] -> String
rows xs =
  intercalate "|" $ map (\ys -> concat ["{", intercalate "|" ys, "}"]) padded
  where
    longests = length . maximumBy (comparing length) <$> transpose xs
    padded = zipWith padR longests <$> xs

fontAttr :: (String, String)
fontAttr = ("fontname", "monospace")

maybeToString :: Maybe String -> String
maybeToString (Just s) = s
maybeToString Nothing = ""

map2M :: Monad m => (a -> c) -> (a -> m d) -> [a] -> m [(c, d)]
map2M _ _ [] = pure []
map2M f g (x:xs) = do
  y <- g x
  rs <- map2M f g xs
  pure ((f x, y) : rs)

padR :: Int -> String -> String
padR n s
  | length s < n = s ++ replicate (n - length s) ' '
  | otherwise = s
