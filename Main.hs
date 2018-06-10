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
import Text.Pretty.Simple

data Options = Options {
      debugTemplate :: Bool
    , template :: FilePath
    , jsonInput :: Maybe FilePath
    }

options :: Parser Options
options = Options
  <$> switch (short 'd' <> help "Debug parsed template and data")
  <*> strArgument (metavar "TEMPLATE")
  <*> optional (strArgument (metavar "JSONFILE" <> help "Use - for stdin"))

opts :: ParserInfo Options
opts = info (helper <*> options)
          (fullDesc <> header "jst" <> progDesc "simple json text templating")

main :: IO ()
main = do
  o <- execParser opts
  tmpl <- T.readFile $ template o
  let blocks = either error id $ parseBlocks tmpl
  s <- case jsonInput o of
          Just "-" -> Just <$> BL8.getContents 
          Just fp -> Just <$> BL8.readFile fp
          Nothing -> pure Nothing
  case s of
    Just s' -> do 
        let v :: Value = either error id . eitherDecode $ s'
        T.putStr $ evalTemplate v blocks
    Nothing -> pure ()
  when (debugTemplate o) $ do
      T.putStrLn "<parsed template>"
      pPrintNoColor blocks


