module Main where

import Data.Map as Map

data Expr = Var String | Abs (String, Expr) | App (Expr, Expr) deriving(Show, Eq, Ord)

newtype Value = Closure (Context, String, Expr) deriving(Show)

type Context = Map String Value

eval :: Expr -> Context -> Maybe Value
eval expr context = case expr of
    Var name -> Map.lookup name context
    Abs (param, body) -> Just $ Closure (context, param, body)
    App (func, arg) -> do
        (Closure (ctx, param, body)) <- eval func context
        evaluedArg <- eval arg context
        eval body (Map.insert param evaluedArg ctx)

myId :: Expr
myId = Abs ("x", Var "x")

callId :: Expr
callId = App (myId, Abs ("y", Var "y"))

main :: IO ()
main = case eval callId Map.empty of
    Just x -> print x
    Nothing -> pure ()