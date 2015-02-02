module StateEvalAndParse where
import Data.Char
import Control.Monad.State
import Debug.Trace
data Expr = Var Char
          | N Int
          | Add Expr Expr
          | Sub Expr Expr
          | Assign Char Expr
          deriving Show

type ParseState = State String

parse :: String -> ParseState Expr
--parse (ch:chs) | trace ("parse, ch: " ++ (show ch) ++ " chs: " ++ chs) False = undefined
parse (ch:chs)
  | isAlpha(ch) = do
    put chs
    return (Var ch)
  | isDigit(ch) = do
    put chs
    return (N (fromEnum ch - fromEnum '0'))
  | ch == '(' = do
    e1 <- parse(chs)
    rest1 <- get
    let (op:rest2) = rest1
    e2 <- parse(rest2)
    rest3 <- get
    let (')':restFinal) = rest3
    put restFinal
    return $ apply op e1 e2
  | ch =='<' = do
    let (v:':':rest) = chs
    e <- parse(rest)
    restFinal <- get
    put restFinal
    return (Assign v e)
      where apply '+' = Add
            apply '-' = Sub

prsStr = "(((<x:5+2)-4)+(<y:2+x))"

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

