module Main where
import Parser
import qualified Data.Text.IO as T

main :: IO ()
main = do
  s <- T.getContents
  print $ parseTemplate s 
