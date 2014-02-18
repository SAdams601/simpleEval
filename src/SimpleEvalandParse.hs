module SimpleEvalAndParse where
import Data.Char

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

eval :: Expr -> Env -> (Int,Env)

eval (Var v) env = (head [val | (x,val) <- env, x==v], env)

eval (N n) env = (n,env)

eval (Add e1 e2) env = (v1+v2,env2)
   where
      (v1,env1) = eval e1 env
      (v2,env2) = eval e2 env1

eval (Sub e1 e2) env = (v1-v2,env2)
   where
      (v1,env1) = eval e1 env
      (v2,env2) = eval e2 env1

eval (Assign x e) env = (v, (x,v):env1)
   where
      (v,env1) = eval e env
