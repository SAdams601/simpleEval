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

type Env = [(Char,Int)]

type EvalSt = State Env

eval :: Expr -> EvalSt (Int)
eval (Var v) = do 
  env <- get
  return (head [val | (x,val) <- env, x==v])

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

evalM :: Expr -> EvalSt (Maybe Int)
evalM (Var v) = do
  env <- get
  return $ Just (head [val | (x,val) <- env, x==v])

evalM (N n) = return (Just n)

evalM (Neg e) = 
  {-do
  mv <- evalM e
  return $ mv >>= (\v -> return (-v))-}
  evalM e >>= (\mv -> return $ mv >>= (\v -> return (-v)))

evalM (Add e1 e2) = 
  evalM e1 >>= (\mv1 ->
    evalM e2 >>= (\mv2 ->
      return $ mv1 >>= (\v1 -> mv2 >>= (\v2 -> return (v1 + v2)))))

evalM (Sub e1 e2) = do
  evalM e1 >>= (\mv1 ->
    evalM e2 >>= (\mv2 ->
      return $ mv1 >>= (\v1 -> mv2 >>= (\v2 -> return (v1 - v2)))))

evalM (Div e1 e2) = do
  evalM e1 >>= (\mv1 ->
    evalM e2 >>= (\mv2 ->
      return $ mv1 >>= 
        (\v1 -> mv2 >>= 
          (\v2 -> case v2 of
                    0 -> Nothing
                    otherwise -> return (v1 `div` v2)))))

--evalM (Add e1 e2) =  evalM e1 >>= (\v1 -> evalM e2 >>= (\v2 -> return (v1 + v2))) 

{- 

Same code as above just using do notation

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

-}
evalM (Assign x e) = do
  env <- get
  (Just v) <- evalM e
  put $ (x,v):env
  return (Just v) 