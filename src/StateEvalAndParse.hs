module StateEvalAndParse where
import Data.Char
import Control.Monad.State

data Expr = Var Char
          | N Int
          | Add Expr Expr
          | Sub Expr Expr
          | Assign Char Expr
          deriving Show

parse :: String -> (Expr,String)

parse (ch:chs) 
  | isAlpha(ch) = ((Var ch),chs)
  | isDigit(ch) = (N (fromEnum ch - fromEnum '0'), chs)
  | ch=='(' = (apply op, restFinal)
  | ch=='<' = (Assign v e, restFinal2)
      where
         (e1,rest1) = parse(chs)
         (op:rest2) = rest1
         (e2,rest3) = parse(rest2)
         (')':restFinal) = rest3
         apply '+' = Add e1 e2
         apply '-' = Sub e1 e2
         (v:':':rest4) = chs
         (e,restFinal2) = parse(rest4)

(ex1,_) = parse("(<x:2+(3-x))")

env1 = []

type Env = [(Char,Int)]

type EvalSt = State Env

eval :: Expr -> EvalSt Int
eval (Var v) = do 
  env <- get
  return (head [val | (x,val) <- env, x==v])

eval (N n) = return n

eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)

eval (Sub e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 - v2)

eval (Assign x e) = do
  env <- get
  v <- eval e
  put $ (x,v):env
  return v 

