module Main where

import Data.Map as Map

data Expr = Int Int | Var String | Abs (String, Expr) | App (Expr, Expr) deriving (Show, Eq, Ord)

data Value = VInt Int | VClosure (Context, String, Expr) deriving (Show)

type Context = Map String Value

eval :: Expr -> Context -> Either String Value
eval expr context = case expr of
  Int n -> Right $ VInt n
  Var name -> case Map.lookup name context of
    Just x -> Right x
    Nothing -> Left "Variable not found"
  Abs (param, body) -> Right $ VClosure (context, param, body)
  App (func, arg) -> do
    funcValue <- eval func context
    argValue <- eval arg context
    case funcValue of
      (VClosure (ctx, param, body)) -> eval body (Map.insert param argValue ctx)
      VInt _ -> Left "Int is not a function (Javascript lol)"

myId :: Expr
myId = Abs ("x", Var "x")

callId :: Expr
callId = App (myId, Int 1)

-- Church Boolean (True)
-- λt.λf.t
true :: Expr
true = Abs ("t", Abs ("f", Var "t"))

-- Church Boolean (False)
-- λt.λf.f
false :: Expr
false = Abs ("t", Abs ("f", Var "f"))

-- Church Conditional (If/Else)
-- λp.λa.λb.p a b
ifelse :: Expr
ifelse = Abs ("p", Abs ("a", Abs ("b", App (App (Var "p", Var "a"), Var "b"))))

callIfelse :: Expr
callIfelse = App (App (App (ifelse, false), Int 1), Int 2)

main :: IO ()
main = case eval callIfelse Map.empty of
  Right (VClosure (_, _, x)) -> print x
  Right (VInt n) -> print n
  Left err -> putStrLn err