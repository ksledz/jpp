module Eval (
	Data(..),
	Expr(..),
	Primitive(..),
	eval,
	test1
) where

import qualified Data.Map.Lazy as Map
import Data.Maybe
import Control.Monad

type Interpreter = Either String Data

type Env = Map.Map String Interpreter

data Primitive = Primitive Int ([Interpreter] -> Interpreter)
instance Show Primitive where
	show (Primitive x _) = "primitive" ++ show x


data Data = DInt Int
	| DBool Bool
	| DLambda String Expr Env
	| DPrimitive Primitive Int [Interpreter]
	deriving Show

data Expr = EVar String
	| EData Data
	| ELambda String Expr
	| EApply Expr Expr
	| ELet [(String, Expr)] Expr
	| EUndefined
	deriving Show

eval :: Expr -> Env -> Interpreter

eval (EData x) _  = return x

eval (ELambda s e) env = return (DLambda s e env)

eval (EVar s) env = case (Map.lookup s env) of
	Nothing -> Left "No such variable"
	Just x -> x

eval (EApply e1 e2) env = do
	d1 <- eval e1 env
	let d2 = eval e2 env
	case d1 of
		DLambda s e lambdaEnv -> eval e (Map.insert s d2 lambdaEnv)
		DPrimitive prim@(Primitive i1 fun) i list -> if (i+1) == i1
			then fun (d2:list)
			else return $ DPrimitive prim (i+1) (d2:list)
		_ -> Left "Tried to perform application on a non-function"


eval (ELet x e1) env =
	let newEnv = foldl (\env1 (s,e) -> Map.insert s (eval e newEnv) env1) env x
	in do
		eval e1 newEnv

eval EUndefined _ = Left "Undefined statement"



test1 = eval (EData $ DInt 5) Map.empty
