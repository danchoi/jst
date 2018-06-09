module Template where
import Types
import Data.Aeson
import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text (Text)
import qualified Data.Text as T


parseTemplate :: Text -> Either String [Block]
parseTemplate s = 
    parseOnly (many' parseBlock) s

-- parser

parseBlock :: Parser Block
parseBlock = 
      parseLoop
  <|> parseConditional
  <|> parseLit

parseLoop :: Parser Block
parseLoop = 
  Loop 
    <$> (obrace *> token "loop" *> parseExpr <* cbrace)
    <*> many' parseBlock
    <* parseEnd

parseConditional :: Parser Block
parseConditional = 
  Conditional
    <$> (obrace *> token "if" *> parseExpr <* cbrace)
    <*> many' parseBlock
    <* parseEnd

parseLit :: Parser Block
parseLit = 
  Literal <$> takeWhile1 (notInClass "{")


token :: Text -> Parser Text
token s = skipSpace *> string s <* skipSpace

obrace :: Parser ()
obrace = string "{{" >> skipSpace *> pure ()

cbrace :: Parser ()
cbrace = skipSpace >> string "}}" *> pure ()

parseExpr :: Parser Expr
parseExpr = 
  takeWhile1 (notInClass " {}")

parseEnd :: Parser ()
parseEnd = pure () <* string "{{end}}" 

