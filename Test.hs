module Main where
import Test.HUnit
import Expr
import Template
import Data.Attoparsec.Text


main :: IO Counts
main = runTestTT . test $ [

    "parseLoop" ~: 
        let inp = parseOnly parseLoop "{{loop foo}}x{{end}}"
            exp = Loop (Expr "foo") [Literal "x"]
        in Right exp @?= inp


  ]
