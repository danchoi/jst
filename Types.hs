module Types where
import Data.Text (Text)
import Data.Aeson

data Block = 
      Loop Expr [Block]
    | Conditional Expr [Block]
    | Literal Text
    deriving (Show, Eq)

data LoopExpr = LoopExpr
  deriving Show


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


