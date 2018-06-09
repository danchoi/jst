module Expr where
import Types
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Aeson
import Control.Lens hiding (Context)
import Data.Aeson.Lens 
import Data.Attoparsec.Text

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


