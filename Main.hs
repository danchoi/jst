module Main where
import Lib
import qualified Data.Text.IO as T

main :: IO ()
main = do
  s <- T.getContents
  print $ parseTemplate s 
