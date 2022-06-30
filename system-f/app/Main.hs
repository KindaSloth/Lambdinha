module Main where

import Data.Map as Map

data Type
  = TInt
  | TArrow Type Type
  | TypeVar String
  | TForall String Type
  deriving (Show, Eq, Ord)

data Expr
  = Int Int
  | Var String
  | Abs (String, Expr, Type)
  | App (Expr, Expr)
  | TypeAbs String Expr
  | TypeApp Expr Type
  deriving (Show, Eq, Ord)

data Value = VInt Int | VClosure (VContext, String, Expr)
  deriving (Show)

type TContext = Map String Type

type VContext = Map String Value

equal :: Type -> Type -> Bool
equal TInt TInt = True
equal (TArrow paramA bodyA) (TArrow paramB bodyB) = equal paramA paramB && equal bodyA bodyB
equal (TypeVar nameA) (TypeVar nameB) = nameA == nameB
equal (TForall paramA forallTypeA) (TForall paramB forallTypeB) =
  let x = typeSubstitute forallTypeB paramB (TypeVar paramA)
   in equal forallTypeA x
equal _ _ = False

typeSubstitute :: Type -> String -> Type -> Type
typeSubstitute t from to = case t of
  TInt -> TInt
  TArrow param body -> TArrow (typeSubstitute param from to) (typeSubstitute body from to)
  TypeVar name -> if name == from then to else TypeVar name
  TForall param forallType -> if param == from then TForall param forallType else TForall param (typeSubstitute forallType from to)

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
      (TArrow paramT bodyT) -> if equal argType paramT then Right bodyT else Left "TypeMismatch"
      _ -> Left "TypeMismatch"
  TypeAbs name term -> case infer term context of
    Right x -> Right $ TForall name x
    _ -> Left "TypeMismatch"
  TypeApp term t -> do
    exprT <- infer term context
    case exprT of
      TForall param forallType -> Right $ typeSubstitute forallType param t
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
  TypeAbs t term -> eval term context
  TypeApp term t -> eval term context

myId :: Expr
myId = TypeAbs "a" (Abs ("x", Var "x", TypeVar "a"))

appId :: Expr
appId = TypeApp myId TInt

-- Church Boolean (True)
-- λt.λf.t
true :: Expr
true = TypeAbs "a" (Abs ("t", Abs ("f", Var "t", TypeVar "a"), TypeVar "a"))

-- Church Boolean (False)
-- λt.λf.f
false :: Expr
false = TypeAbs "a" (Abs ("t", Abs ("f", Var "f", TypeVar "a"), TypeVar "a"))

-- Church Conditional (If/Else)
-- λp.λa.λb.p a b
ifelse :: Expr
ifelse = TypeAbs "a" (Abs ("p", Abs ("a", Abs ("b", App (App (Var "p", Var "a"), Var "b"), TypeVar "a"), TypeVar "a"), TArrow (TypeVar "a") (TArrow (TypeVar "a") (TypeVar "a"))))

appTrue :: Expr
appTrue = TypeApp true TInt

appFalse :: Expr
appFalse = TypeApp false TInt

appIfelse :: Expr
appIfelse = TypeApp ifelse TInt

callIfelse :: Expr
callIfelse = App (App (App (appIfelse, appTrue), Int 1), Int 2)

main :: IO ()
main = case infer callIfelse Map.empty of
  Right _ -> case eval callIfelse Map.empty of
    Right x -> print x
    Left err' -> print err'
  Left err -> print err
