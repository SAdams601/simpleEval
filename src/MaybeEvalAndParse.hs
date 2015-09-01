{-# LANGUAGE Rank2Types,LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts #-}

module MaybeEvalAndParse where
import Data.Char
import Control.Monad.State
import Debug.Trace
data Expr = Var Char
          | N Int
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Assign Char Expr
          | Div Expr Expr
          deriving Show

type ParseState = State String

parse :: String -> ParseState Expr
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
  | ch =='-' = do
    e <- parse(chs)
    return (Neg e) 
  | ch =='<' = do
    let (v:':':rest) = chs
    e <- parse(rest)
    restFinal <- get
    put restFinal
    return (Assign v e)
      where apply '+' = Add
            apply '-' = Sub
            apply '/' = Div

prsStr = "(((<x:5+2)-4)+(<y:2+x))"
divStr = "(4/2)"


env1 = []

type Env a = [(Char, a)]

type EvalSt a = State (Env a) a


eval :: Expr -> EvalSt Int
eval (Var v) = do 
  env <- get
  let res = (head [val | (x,val) <- env, x==v]) 
  return res

eval (N n) = return n
eval (Neg e) = do
  v <- eval e
  return (-v)
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)

eval (Sub e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 - v2)

eval (Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 `div` v2)

eval (Assign x e) = do
  env <- get
  v <- eval e
  put $ (x,v):env
  return v

evalM :: (Monad m, Show (m Int)) => Expr -> EvalSt (m Int)
evalM (Var v) = do
  env <- get
  return (head [val | (x,val) <- env, x==v])

evalM (N n) = return (return n)

evalM (Neg e) = 
  evalM e >>= (\mv -> return $ mv >>= (\v -> return (-v)))

evalM (Add e1 e2) = 
  evalM e1 >>= (\mv1 ->
    evalM e2 >>= (\mv2 ->
      return $ mv1 >>= (\v1 -> mv2 >>= (\v2 -> return (v1 + v2)))))

evalM (Sub e1 e2) =
  evalM e1 >>= (\mv1 ->
    evalM e2 >>= (\mv2 ->
      return $ mv1 >>= (\v1 -> mv2 >>= (\v2 -> return (v1 - v2)))))

evalM (Div e1 e2) =
  evalM e1 >>= (\mv1 ->
    evalM e2 >>= (\mv2 ->
      return $ mv1 >>= 
        (\v1 -> mv2 >>= 
          (\v2 -> case v2 of
            --changed Nothing to fail here
                    0 -> fail "Divide by zero"
                    otherwise -> return (v1 `div` v2)))))
evalM (Assign x e) = do
 env <- get
 evalM e >>= (\mv -> put ((x,mv):env) >> return mv)

{- 

Same code as above just using do notation. I've kept the bind calls to handle the nothing case more smoothly.
Otherwise the Neg, Add, and Sub cases would need the case statements much like the following:

case mv of
  Nothing -> Nothing
  Just v -> return (-v)

Sub and Add would need nested cases to handle both mv1 and mv2.

evalM (Neg e) = 
  mv <- evalM
  return $ mv >>= (\v -> return (-v))

evalM (Add e1 e2) = do
  mv1 <- evalM e1
  mv2 <- evalM e2
  return $ mv1 >>= (\v1 -> mv2 >>= (\v2 -> return (v1 + v2)))

evalM (Sub e1 e2) = do
  mv1 <- evalM e1
  mv2 <- evalM e2
  return $ mv1 >>= (\v1 -> mv2 >>= (\v2 -> return (v1 - v2)))

evalM (Div e1 e2) = do
  mv1 <- evalM e1
  mv2 <- evalM e2
  return $ mv1 >>= 
    (\v1 -> mv2 >>= 
      (\v2 -> case v2 of
        0 -> Nothing
        otherwise -> return (v1 `div` v2)))

Neg case written for MonadPlus

evalM (Div e1 e2) = do
  mv1 <- evalM e1
  mv2 <- evalM e2
  return $ mv1 >>= 
    (\v1 -> mv2 >>= 
      (\v2 -> case v2 of
        0 -> mzero
        otherwise -> return (v1 `div` v2)))

-}

