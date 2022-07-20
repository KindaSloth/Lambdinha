module Main where

import Data.Map as Map

data Kind = Star | KArrow Kind Kind
  deriving (Show, Eq, Ord)

data Type
  = TInt
  | TypeVar String
  | TArrow Type Type
  | TForall (String, Kind) Type
  | TAbs (String, Kind) Type
  | TApp Type Type
  deriving (Show, Eq, Ord)

data Expr
  = Int Int
  | Var String
  | Abs (String, Expr, Type)
  | App (Expr, Expr)
  | TypeAbs (String, Kind) Expr
  | TypeApp Expr Type
  deriving (Show, Eq, Ord)

data Value = VInt Int | VClosure (VContext, String, Expr)
  deriving (Show)

type KContext = Map String Kind

type TContext = Map String Type

type VContext = Map String Value

-- kindEqual :: Kind -> Kind -> Bool
-- kindEqual Star Star = True
-- kindEqual (KArrow paramA bodyA) (KArrow paramB bodyB) = kindEqual paramA paramB && kindEqual bodyA bodyB
-- kindEqual _ _ = False

kindChecker :: Type -> KContext -> Either String Kind
kindChecker t kContext = case t of
  TInt -> pure Star
  TypeVar name -> case Map.lookup name kContext of
    Just x -> pure x
    _ -> Left "KindMismatch"
  TArrow param body -> case kindChecker param kContext of
    Right Star -> case kindChecker body kContext of
      Right Star -> pure Star
      _ -> Left "KindMismatch"
    _ -> Left "KindMismatch"
  TForall (param, k) forallType -> kindChecker forallType (Map.insert param k kContext)
  TAbs (param, kParam) bodyType -> case kindChecker bodyType (Map.insert param kParam kContext) of
    Right x -> Right $ KArrow kParam x
    Left err -> Left err
  TApp tFunc tArg -> do
    kFunc <- kindChecker tFunc kContext
    kArg <- kindChecker tArg kContext
    case kFunc of
      KArrow kParam kBody -> if kArg == kParam then Right kBody else Left "KindMismatch"
      _ -> Left "KindMismatch"

-- equal :: Type -> Type -> Bool
-- equal TInt TInt = True
-- equal (TArrow paramA bodyA) (TArrow paramB bodyB) = equal paramA paramB && equal bodyA bodyB
-- equal (TypeVar nameA) (TypeVar nameB) = nameA == nameB
-- equal (TForall (paramA, _) forallTypeA) (TForall (paramB, _) forallTypeB) =
--   let x = typeSubstitute forallTypeB paramB (TypeVar paramA)
--    in equal forallTypeA x
-- equal _ _ = False

typeSubstitute :: Type -> String -> Type -> Type
typeSubstitute t from to = case t of
  TInt -> TInt
  TArrow param body -> TArrow (typeSubstitute param from to) (typeSubstitute body from to)
  TypeVar name -> if name == from then to else TypeVar name
  TForall param@(x, _) forallType -> if x == from then TForall param forallType else TForall param (typeSubstitute forallType from to)
  _ -> t

infer :: Expr -> TContext -> KContext -> Either String Type
infer expr tContext kContext = case expr of
  Int _ -> Right TInt
  Var name -> case Map.lookup name tContext of
    Just x -> Right x
    Nothing -> Left "TypeMismatch"
  Abs (param, body, t) -> case infer body (Map.insert param t tContext) kContext of
    Right x -> do
      tK <- kindChecker t kContext
      if tK == Star then Right $ TArrow t x else Left "KindMismatch"
    Left err -> Left err
  App (func, arg) -> do
    funcType <- infer func tContext kContext
    argType <- infer arg tContext kContext
    case funcType of
      (TArrow paramT bodyT) -> if argType == paramT then Right bodyT else Left "TypeMismatch"
      _ -> Left "TypeMismatch"
  TypeAbs (name, k) term -> infer term tContext (Map.insert name k kContext)
  TypeApp term t -> do
    exprT <- infer term tContext kContext
    case exprT of
      TForall (param, paramK) forallType -> do
        tK <- kindChecker t kContext
        if tK == paramK then Right $ typeSubstitute forallType param t else Left "KindMismatch"
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

pairTyp :: Type
pairTyp =
  TAbs
    ("a", Star)
    ( TAbs
        ("b", Star)
        ( TForall
            ("r", Star)
            (TArrow (TArrow (TypeVar "a") (TypeVar "b")) (TypeVar "r"))
        )
    )

main :: IO ()
main = case kindChecker pairTyp Map.empty of
  Right x -> print x
  Left err -> print err
