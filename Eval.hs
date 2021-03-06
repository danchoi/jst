module Eval where
import Types
import Data.Monoid
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Aeson
import Control.Lens hiding (Context)
import Data.Aeson.Lens 
import Data.Attoparsec.Text
import Data.Scientific (Scientific)

evalTemplate :: Value -> [Block] -> Text
evalTemplate v bs = evalBlocks v bs

evalBlocks :: Value -> [Block] -> Text
evalBlocks v bs = 
  let c = Context v []
  in mconcat [ evalBlock c b | b <- bs ]

evalBlock :: Context -> Block -> Text
evalBlock _ (Literal s) = s
evalBlock c (Interpolate e) = evalS $ evalContext c e 
evalBlock c@(Context v st) (Loop key e bs) = 
    let vs = evalContext c e ^.. values
    in T.concat $ 
          [ let extra = [ (key, v')
                        , ("$index", toJSON idx)
                        , ("$last", Bool $ idx == length vs)
                        , ("$length", toJSON $ length vs)
                        ]
                c' = Context v (extra <> st)
            in evalBlock c' b
          | (idx, v') <- zip [(1 :: Int)..] vs, b <- bs ]
evalBlock c (Conditional ifBranch ifElses elseBlocks) =
    let 
        bs :: [First Text]
        bs = [ let v = evalContext c expr
                in First $
                    if truthy v
                    then Just $ T.concat $ map (evalBlock c) blocks
                    else Nothing
              | (expr, blocks) <- ifBranch:ifElses ]
        elseOut :: Text
        elseOut = T.concat $ 
                      [ evalBlock c b | b <- elseBlocks ] 
        chosenBranch :: Maybe Text
        chosenBranch = getFirst $ mconcat bs
    in fromMaybe elseOut chosenBranch

truthy :: Value -> Bool
truthy (Bool False) = False
truthy Null = False
truthy (String "") = False
truthy _ = True


-- evaluation

evalContext :: Context -> Expr -> Value
evalContext (Context v m) (VarExpr Nothing p) = eval v p
evalContext (Context _ m) (VarExpr (Just k) p) = 
    let v = fromMaybe Null $ lookup k m
    in eval v p
evalContext (Context _ m) (LoopVar k) = 
    fromMaybe Null $ lookup k m

evalContext c (NegExpr e) = Bool . not . truthy $ evalContext c e
evalContext c (LitExpr v) = v
evalContext c (BinaryExpr op e1 e2) = 
      evalBinary op e1 e2
    where
      v1 = evalContext c e1
      v2 = evalContext c e2

      evalBinary :: BinaryOp -> Expr -> Expr -> Value
      evalBinary And x y = if truthy v1 then v2 else (Bool False)
      evalBinary Or x y = if truthy v1 then v1 else v2
      evalBinary Equal x y = Bool $ v1 == v2 
      evalBinary NotEqual x y = Bool $ v1 /= v2 
      evalBinary Sub x y = doMath (-)
      evalBinary Add x y = doMath (+)
      evalBinary Mult x y = doMath (*)
      evalBinary Div x y = doMath (/)

      doMath :: (Scientific -> Scientific -> Scientific) -> Value
      doMath op = maybe Null Number $ op <$> toNum v1 <*> toNum v2

      toNum :: Value -> Maybe Scientific
      toNum v = v ^? _Number 

eval :: Value -> Path -> Value
eval v (Key k) = fromMaybe Null $ v ^? key k
eval v@(Array _) UnpackArray = v
eval v UnpackArray = toJSON [v] -- just stick the item in a singleton

evalS :: Value -> Text
evalS (String s) = s
evalS v@(Number n) = 
    maybe "?" T.pack $ 
          v ^? _Integral . to show
      <|> v ^? _Double . to show
evalS (Bool n) = T.toLower . T.pack . show $ n
evalS v = fromMaybe "?" $ v ^? _String


