module Main where
import Template
import qualified Data.Text.IO as T

main :: IO ()
main = do
  s <- T.getContents
  print $ parseTemplate s 
