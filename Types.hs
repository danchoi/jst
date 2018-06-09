module Types where
import Data.Text (Text)

data Block = 
      Loop Expr [Block]
    | Conditional Expr [Block]
    | Literal Text
    deriving (Show, Eq)

type Expr = Text

data LoopExpr = LoopExpr
  deriving Show


