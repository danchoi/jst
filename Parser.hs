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
    <*> (token "in" *> pExpr <* cbrace)
    <*> many' parseBlock
    <* parseEnd

eatRestOfLine :: Parser ()
eatRestOfLine = do
  _ <- takeWhile (inClass " \t")
  _ <- optional (char '\n')
  return ()

parseConditional :: Parser Block
parseConditional = do
  _ <- obrace >> token "if" 
  Conditional
    <$> (pExpr <* cbrace)
    <*> many' parseBlock
    <* parseEnd

parseInterpolate :: Parser Block
parseInterpolate =
    Interpolate <$> (obrace *> pExpr <* cbrace)

parseLit :: Parser Block
parseLit = 
  Literal <$> takeWhile1 (notInClass "{") <?> "parseLit"

token :: Text -> Parser Text
token s = skipSpace *> string s <* skipSpace

obrace :: Parser ()
obrace = string "{{" >> skipSpace *> pure ()

cbrace :: Parser ()
cbrace = do
  -- -}} eats rest of line
  -- }} does not
  _ <- skipSpace 
  (string "-}}" *> pure () <* eatRestOfLine)
  <|>
  (string "}}" *> pure ())


parseEnd :: Parser ()
parseEnd = 
  obrace >> token "end" >> cbrace

-- parsers

pExpr :: Parser Expr
pExpr = 
        pBinaryExprs
    <|> inParens pExpr
    <|> pNegExpr
    <|> pLoopVar 
    <|> pVarExpr
    <|> pLitExpr 

pBinaryExprs :: Parser Expr
pBinaryExprs = 
    -- in reverse order of precedence
        pBinaryExpr [And, Or]
    <|> pBinaryExpr [NotEqual, Equal]

pVarExpr :: Parser Expr
pVarExpr = Expr <$> (optional pVar) <*> pPath

pNegExpr :: Parser Expr
pNegExpr =
    NegExpr <$> (token "!" *> (inParens pExpr <|> atoms))
  where atoms = pLoopVar 
            <|> pVarExpr
            <|> pLitExpr 
    

pBinaryExpr :: [BinaryOp] -> Parser Expr
pBinaryExpr ops = do
    e1 <- operand1
    op <- pBinaryOp ops
    e2 <- operand2
    pure $ BinaryExpr op e1 e2
  where operand1 = inParens pExpr 
                  <|> pExprNotBinary 
        operand2 = inParens pExpr 
                  <|> pBinaryExprs
                  <|> pExprNotBinary 
        pExprNotBinary = 
            pNegExpr
            <|> pLoopVar
            <|> pVarExpr
            <|> pLitExpr 

pBinaryOp :: [BinaryOp] -> Parser BinaryOp
pBinaryOp ops = choice 
  $ map (\op -> token (toToken op) *> pure op) ops

toToken :: BinaryOp -> Text
toToken And = "&&"
toToken Or = "||"
toToken Equal = "=="
toToken NotEqual = "!="

pLitExpr :: Parser Expr
pLitExpr = LitExpr <$> 
    choice [ 
      Number . read <$> many1 digit
    , String <$> litString
    , Bool <$> (    (string "true" *> pure True) 
                <|> (string "false" *> pure False))
    ]

litString :: Parser Text
litString = 
    (char '"' *> takeWhile (notInClass "\"") <* char '"') 
    <|> 
    (char '\'' *> takeWhile (notInClass "'") <* char '\'')


inParens :: Parser a -> Parser a
inParens p = char '(' *> p <* char ')'

pLoopVar :: Parser Expr
pLoopVar = do
  skipSpace 
  x <- char '$'
  xs <- pVar
  pure $ LoopVar (x `T.cons` xs)
    

pPath :: Parser Path
pPath = pKey <|> pArr

pArr :: Parser Path
pArr = 
    stripWhiteSpace (
      char '.' >> skipSpace >> string "[]" >> pure UnpackArray

    )

pKey :: Parser Path
pKey = do
    _ <- skipSpace >> char '.' >> skipSpace
    Key <$> pVar

stripWhiteSpace :: Parser a -> Parser a
stripWhiteSpace p = skipSpace *> p <* skipSpace

pVar :: Parser Text
pVar = do
  skipSpace
  x <- satisfy (inClass "$_A-Za-z")
  xs <- takeWhile (inClass "A-Za-z0-9_")
  skipSpace
  pure $ x `T.cons` xs

