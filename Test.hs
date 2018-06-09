module Main where
import Test.HUnit
import Expr
import Template
import Data.Attoparsec.Text
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens hiding (Context)
import qualified Data.Map as M


main :: IO Counts
main = runTestTT . test $ [

    "parseLoop" ~: 
        let inp = parseOnly parseLoop "{{loop foo}}x{{end}}"
            exp = Loop "foo" [Literal "x"]
        in Right exp @?= inp

  , "parse key path" ~:
        let inp = parseOnly pExpr ".foo"
            exp = Expr Nothing (Key "foo")
        in Right exp @?= inp

  , "parse context and key path" ~:
        let inp = parseOnly pExpr "bar.foo"
            exp = Expr (Just "bar") (Key "foo")
        in Right exp @?= inp

  , "parse unpack array path" ~:
        let inp = parseOnly pExpr ".[]"
            exp = Expr Nothing UnpackArray
        in Right exp @?= inp

  , "eval" ~:
        let v = "{\"a\": \"b\"}" ^?! _Value
        in eval v (Key "a") @=? String "b"

  , "evalContext" ~:
        let v = "{\"a\": \"b\"}" ^?! _Value
            context = Context Null [("item", v)]
            expr = Expr (Just "item") (Key "a")
        in evalContext context expr @=? String "b"
  ]
