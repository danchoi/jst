module Main where
import Test.HUnit
import Types
import Eval
import Parser
import Data.Attoparsec.Text
import Data.Aeson.Lens
import Data.Aeson
import Data.Text (Text)
import Control.Lens hiding (Context)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe

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
        @?= Right [Conditional (LoopVar "$last") [Literal " and "]]

  , "parse {{if $last}} and {{end}}{{item.name}}" ~:
        parseOnly (many' parseBlock) "{{if $last}} and {{end}}{{item.name}}"
        @?= Right [Conditional (LoopVar "$last") [Literal " and "],Interpolate (Expr (Just "item") (Key "name"))]


  , "parse key path" ~:
        let inp = parseOnly pExpr ".foo"
            exp = Expr Nothing (Key "foo")
        in Right exp @?= inp

  , "parse key path one letter" ~:
        let inp = parseOnly pExpr ".f"
            exp = Expr Nothing (Key "f")
        in Right exp @?= inp

  , "parse context and key path" ~:
        let inp = parseOnly pExpr "bar.foo"
            exp = Expr (Just "bar") (Key "foo")
        in Right exp @?= inp

  , "parse unpack array path" ~:
        let inp = parseOnly pExpr ".[]"
            exp = Expr Nothing UnpackArray
        in Right exp @?= inp

  , "parse conditional" ~:
        parseOnly parseConditional "{{if $last}}{{end}}"
        @?=
        Right (Conditional (LoopVar "$last") [])

  , "eval" ~:
        let v = "{\"a\": \"b\"}" ^?! _Value
        in eval v (Key "a") @=? String "b"

  , "evalContext" ~:
        let v = "{\"a\": \"b\"}" ^?! _Value
            context = Context Null [("item", v)]
            expr = Expr (Just "item") (Key "a")
        in evalContext context expr @=? String "b"

  , "eval test 1" ~:
        evalTest "{\"a\": \"b\"}" ".a"
        @?= String "b"
         
  , "eval test: bool true" ~:
        evalTest "{\"a\": true}" ".a" @?= Bool True

  , "eval test: bool negate true" ~:
        evalTest "{\"a\": true}" "! .a" @?= Bool False 

  , "eval test: !=" ~:
        evalTest "{\"a\": true, \"b\": false}" ".a != .b" 
          @?= Bool True

  , "parse expr: == with lit" ~:
        parseExpr ".a == \"foo\"" 
          @?= BinaryExpr Equal 
                (Expr Nothing (Key "a"))
                (LitExpr (String "foo"))

  , "parse precedence with parens" ~:
        parseExpr "(! .a) && (.b != 2)"
          @?= BinaryExpr And 
                  (NegExpr (Expr Nothing (Key "a"))) 
                  (BinaryExpr NotEqual 
                      (Expr Nothing (Key "b")) 
                      (LitExpr (Number 2.0)))

  , "parse precedence without parens" ~:
        parseExpr "! .a && .b != 2"
          @?= BinaryExpr And 
                  (NegExpr (Expr Nothing (Key "a"))) 
                  (BinaryExpr NotEqual 
                      (Expr Nothing (Key "b")) 
                      (LitExpr (Number 2.0)))

  , "binary expr with loopVars" ~:
        parseExpr "! $last && $index != 2"
          @?= BinaryExpr And 
                  (NegExpr (LoopVar "$last"))
                  (BinaryExpr NotEqual 
                      (LoopVar "$index")
                      (LitExpr (Number 2.0)))


  , "eval test: == with lit" ~:
        evalTest "{\"a\": \"foo\"}" ".a == \"foo\"" 
          @?= Bool True

  , "eval test: != with lit" ~:
        evalTest "{\"a\": \"foo\"}" ".a != \"foo\"" 
          @?= Bool False
  ]

parseExpr :: Text -> Expr
parseExpr s = either  
      (\e -> error $ "Could not parse " ++ show s ++ " error: " ++ e) id
    $ parseOnly pExpr s

-- | convenient for testing
evalTest :: BL8.ByteString  -- json
         -> Text -- ^ expression
         -> Value
evalTest bs expr =
    let v = fromMaybe (error $ "cannot parse value " ++ show bs) $ bs ^? _Value
        expr' = either 
                  (\e -> error $ "cannot parse expr " ++ show expr ++ show e) id
                  $ parseOnly pExpr expr
        c = Context v []
    in evalContext c expr'


