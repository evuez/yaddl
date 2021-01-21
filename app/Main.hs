module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Lib (mkDocument)
import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)
import Text.Dot (showDot)
import Types (Document, Error)
import Writer.Dot (mkDot)

main :: IO ()
main = do
  (inPath:outPath:_) <- getArgs
  contents <- readFile inPath
  case write outPath contents of
    Left e -> putStrLn ("\x1B[31m" ++ e ++ "\x1B[0m")
    Right x -> writeFile outPath x

write :: FilePath -> String -> Either Error String
write path contents = do
  writer <- getWriter path
  writer <$> mkDocument contents

getWriter :: FilePath -> Either String (Document -> String)
getWriter path =
  case lookup ext writers of
    Just writer -> Right writer
    Nothing -> Left ("No writer found for '" ++ ext ++ "'")
  where
    ext = tail $ takeExtension path

writers :: [(String, Document -> String)]
writers = [("json", unpack . encode), ("dot", showDot . mkDot)]
