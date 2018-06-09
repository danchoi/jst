module Main where
import Test.HUnit
import Types
import Eval
import Parser
import Data.Attoparsec.Text
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens hiding (Context)
import qualified Data.Map as M


main :: IO Counts
main = runTestTT . test $ [

    "parseLoop" ~: 
        let inp = parseOnly parseLoop "{{for item in .foo}}x{{end}}"
            exp = Loop "item" (Expr Nothing (Key "foo")) [Literal "x"]
        in Right exp @?= inp

  , "parse interpolation" ~: 
        let inp = parseOnly parseBlock "{{.foo}}"
            exp = Interpolate (Expr Nothing (Key "foo"))
        in Right exp @?= inp

  , "don't parse {{end}} by itself" ~:
        parseOnly (many' parseBlock) "{{end}}"
        @=? Right []

  , "parse {{if $last}} and {{end}}" ~:
        parseOnly (many' parseBlock) "{{if $last}} and {{end}}"
        @?= Right [Conditional True (LoopVar "$last") [Literal " and "]]

  , "parse {{if $last}} and {{end}}{{item.name}}" ~:
        parseOnly (many' parseBlock) "{{if $last}} and {{end}}{{item.name}}"
        @?= Right [Conditional True (LoopVar "$last") [Literal " and "],Interpolate (Expr (Just "item") (Key "name"))]


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

  , "parse negated conditional" ~:
        parseOnly parseConditional "{{if ! $last}}{{end}}"
        @?=
        Right (Conditional False (LoopVar "$last") [])

  , "parse conditional" ~:
        parseOnly parseConditional "{{if $last}}{{end}}"
        @?=
        Right (Conditional True (LoopVar "$last") [])

  , "eval" ~:
        let v = "{\"a\": \"b\"}" ^?! _Value
        in eval v (Key "a") @=? String "b"

  , "evalContext" ~:
        let v = "{\"a\": \"b\"}" ^?! _Value
            context = Context Null [("item", v)]
            expr = Expr (Just "item") (Key "a")
        in evalContext context expr @=? String "b"
  ]
