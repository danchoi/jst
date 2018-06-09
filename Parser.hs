module Parser where
import Prelude hiding (takeWhile)
import Types
import Data.Aeson
import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text (Text)
import qualified Data.Text as T


parseBlocks :: Text -> Either String [Block]
parseBlocks s = 
    parseOnly (many' parseBlock) s

-- parser

parseBlock :: Parser Block
parseBlock = 
      parseLoop
  <|> parseConditional
  <|> parseInterpolate
  <|> parseLit

-- | for item in expr
parseLoop :: Parser Block
parseLoop = do
  _ <- obrace *> token "for"
  Loop 
    <$> pVar
    <*> (token "in" *> pExpr <* cbrace <* eatRestOfLine)
    <*> many' parseBlock
    <* parseEnd

eatRestOfLine :: Parser ()
eatRestOfLine = do
  _ <- takeWhile (inClass " \t")
  _ <- char '\n'
  return ()

parseConditional :: Parser Block
parseConditional = 
  Conditional
    <$> (obrace *> token "if" *> pExpr <* cbrace)
    <*> many' parseBlock
    <* parseEnd

parseInterpolate :: Parser Block
parseInterpolate =
    Interpolate <$> (obrace *> pExpr <* cbrace)


parseLit :: Parser Block
parseLit = 
  Literal <$> takeWhile1 (notInClass "{")


token :: Text -> Parser Text
token s = skipSpace *> string s <* skipSpace

obrace :: Parser ()
obrace = string "{{" >> skipSpace *> pure ()

cbrace :: Parser ()
cbrace = skipSpace >> string "}}" *> pure ()


parseEnd :: Parser ()
parseEnd = 
  pure () <* string "{{end}}" <* eatRestOfLine


-- parsers

pExpr :: Parser Expr
pExpr = Expr <$> (optional pTarget) <*> pPath

pPath :: Parser Path
pPath = pKey <|> pArr

pArr :: Parser Path
pArr = 
    stripWhiteSpace (
      char '.' >> skipSpace >> string "[]" >> pure UnpackArray

    )

stripWhiteSpace :: Parser a -> Parser a
stripWhiteSpace p = skipSpace *> p <* skipSpace

pKey :: Parser Path
pKey = do
    _ <- skipSpace >> char '.' >> skipSpace
    Key <$> pVar

pVar :: Parser Text
pVar = do
  skipSpace
  x <- satisfy (inClass "_A-Za-z")
  xs <- takeWhile1 (inClass "A-Za-z0-9_")
  skipSpace
  pure $ x `T.cons` xs


pTarget :: Parser Target
pTarget = skipSpace >> takeWhile1 (not . inClass " .")


