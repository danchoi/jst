{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Parser
import Control.Monad (when)
import Eval
import Data.Aeson (eitherDecode, Value)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.Environment
import Options.Applicative
import System.IO

data Options = Options {
      debug :: Bool
    , template :: FilePath
    , jsonInput :: FilePath
    }

options :: Parser Options
options = Options
  <$> switch (short 'v' <> help "Debug parsing")
  <*> strArgument (metavar "TEMPLATE")
  <*> strArgument (metavar "JSONFILE" <> help "Use - for stdin")

opts :: ParserInfo Options
opts = info (helper <*> options)
          (fullDesc <> header "jst" <> progDesc "simple json text templating")

main :: IO ()
main = do
  o <- execParser opts
  tmpl <- T.readFile $ template o
  let bs = either error id $ parseBlocks tmpl
  s <- case jsonInput o of
          "-" -> BL8.getContents 
          fp -> BL8.readFile fp
  let v :: Value = either error id . eitherDecode $ s
  when (debug o) $ do
    print bs
    print v
  T.putStr $ evalTemplate v bs


