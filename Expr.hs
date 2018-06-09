module Expr where
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Aeson
import Control.Lens hiding (Context)
import Data.Aeson.Lens 
import Data.Attoparsec.Text

data Expr = Expr (Maybe Target) Path
  deriving (Show, Eq)

type Target = Text  -- foo.bar : context is "foo"

data Context = Context
    Value
    [(Text, Value)] -- stores the context of loops as a scope stack

data Path = 
    Key Text -- .foo
  | UnpackArray  -- .[]
  deriving (Show, Eq)

-- evaluation

evalContext :: Context -> Expr -> Value
evalContext (Context v m) (Expr Nothing p) = eval v p
evalContext (Context _ m) (Expr (Just k) p) = 
    let v = fromMaybe Null $ lookup k m
    in eval v p

eval :: Value -> Path -> Value
eval v (Key k) = fromMaybe Null $ v ^? key k
eval v UnpackArray = undefined

evalS :: Value -> Text
evalS (String s) = s
evalS v = fromMaybe "?" $ v ^? _String

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


