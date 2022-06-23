module Main where

import Data.Map as Map

data Expr = Int Int | Var String | Abs (String, Expr, Type) | App (Expr, Expr) deriving(Show, Eq, Ord)

data Value = VInt Int | VClosure (VContext, String, Expr) 
    deriving(Show)

data Type = TInt | TArrow Type Type
    deriving(Show, Eq, Ord)

type TContext = Map String Type

type VContext = Map String Value

equal :: Type -> Type -> Bool
equal TInt TInt = True
equal (TArrow paramA bodyA) (TArrow paramB bodyB) = equal paramA paramB && equal bodyA bodyB
equal _ _ = False

infer :: Expr -> TContext -> Either String Type
infer expr context = case expr of
    Int _ -> Right TInt
    Var name -> case Map.lookup name context of
        Just x -> Right x
        Nothing -> Left "TypeMismatch"
    Abs (param, body, t) -> case infer body (Map.insert param t context) of
        Right x -> Right $ TArrow t x
        Left err -> Left err
    App (func, arg) -> do
        funcType <- infer func context
        argType <- infer arg context
        case funcType of
            (TArrow paramT bodyT) -> if argType == paramT then Right bodyT else Left "TypeMismatch" 
            _ -> Left "TypeMismatch"

eval :: Expr -> VContext -> Either String Value
eval expr context = case expr of
    Int n -> Right $ VInt n
    Var name -> case Map.lookup name context of
        Just x -> Right x
        Nothing -> Left "Variable not found"
    Abs (param, body, _) -> Right $ VClosure (context, param, body)
    App (func, arg) -> do
        funcValue <- eval func context
        argValue <- eval arg context
        case funcValue of
            (VClosure (ctx, param, body)) -> eval body (Map.insert param argValue ctx)
            VInt _ -> Left "Int is not a function (Javascript lol)"

intId :: Expr
intId = Abs ("id", Var "id", TInt)

callId :: Expr
callId = App (intId, Int 1)

main :: IO ()
main = case infer callId Map.empty of
    Right _ -> case eval callId Map.empty of
        Right x -> print x
        Left err -> putStrLn err
    Left err -> putStrLn err