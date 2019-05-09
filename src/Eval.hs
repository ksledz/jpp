module Eval (
  Data(..),
  test1
) where

import qualified Data.Map.Strict as Map
import Data.Maybe

type Env = Map.Map String Data

data Primitive = Primitive Int ([Data] -> Data)

data Data = DInt Int
  | DBool Bool
  | DLambda String Expr Env
  | DPrimitive Primitive Int [Data]

data Expr = EVar String
  | EData Data
  | ELambda String Expr
  | EApply Expr Expr
  | ELet [(String, Expr)] Expr

eval :: Expr -> Env -> Data
eval (EData x) _  = x
eval (ELambda s e) env = DLambda s e env
eval (EVar s) env = fromJust $ Map.lookup s env
eval (EApply e1 e2) env = let
  d1 = eval e1 env
  d2 = eval e2 env
  in case d1 of
    DLambda s e lambdaEnv -> eval e $ Map.insert s d2 lambdaEnv
    DPrimitive prim@(Primitive i1 fun) i list -> if (i+1) == i1
      then fun (d2:list)
      else DPrimitive  prim (i+1) (d2:list)
    _ -> undefined --rzucanie błędu

eval (ELet x e1) env = let
  newEnv = foldl (\env1 (s,e) -> Map.insert s (eval e newEnv) env1) env x
  in eval e1 newEnv


test1 = eval (EData $ DInt 5) Map.empty

