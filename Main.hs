{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Parser
import Eval
import Data.Aeson
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.Environment

main :: IO ()
main = do
  tmplFile:_ <- getArgs
  tmpl <- T.readFile tmplFile
  let bs = either error id $ parseBlocks tmpl
  print bs
  v :: Value <- (either error id . eitherDecode) <$> BL8.getContents 
  print v
  T.putStrLn $ evalTemplate v bs

