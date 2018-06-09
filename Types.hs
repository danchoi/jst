module Types where
import Data.Text (Text)
import Data.Aeson

data Block = 
      Loop Text Expr [Block]
    | Conditional Expr [Block]
    | Interpolate Expr
    | Literal Text
    deriving (Show, Eq)

data Expr = Expr (Maybe Target) Path
          | LoopVar Text -- $index, $last
  deriving (Show, Eq)

type Target = Text  -- foo.bar : context is "foo"

data Context = Context
    Value -- root Value
    [(Text, Value)] -- stores the context of loops as a scope stack

data Path = 
    Key Text -- .foo
  | UnpackArray  -- .[]
  deriving (Show, Eq)


