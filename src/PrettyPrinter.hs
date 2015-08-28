module PrettyPrinter where
import StateEvalAndParse
import Control.Monad.State
import MaybeEvalAndParse

data Storage = Storage {
  iState :: Int,
  expr :: Expr
  
                                 } deriving Show
type PrintState = State Int

indent = "   "

getIndentLevel :: Int -> String
getIndentLevel l = foldr (++) "" $ take l $ repeat indent
{-

-}

statePrettyPrint :: Expr -> PrintState String
statePrettyPrint (Var c) = do
  i <- get
  return $ (getIndentLevel i) ++ [c]
statePrettyPrint (N n) = do
  i <- get
  return $ (getIndentLevel i) ++ show n
statePrettyPrint (Assign c e) = do
  i <- get
  let iLevel = findIndent i e
      newLine = case iLevel of
        0 -> ""
        _ -> "\n"
  put iLevel
  prettyExpr <- statePrettyPrint e
  put i
  return $ (getIndentLevel i) ++ "<" ++ [c] ++ ":" ++ newLine ++ prettyExpr
statePrettyPrint (Add e1 e2) = do
  i <- get
  put (i+1)
  prettyE1 <- statePrettyPrint e1
  prettyE2 <- statePrettyPrint e2
  put i
  return $ (getIndentLevel i) ++ "(\n" ++ prettyE1 ++ "\n" ++ (getIndentLevel (i+1))  ++ "+\n" ++ prettyE2 ++ ")"
statePrettyPrint (Sub e1 e2) = do
  i <- get
  put (i+1)
  prettyE1 <- statePrettyPrint e1
  prettyE2 <- statePrettyPrint e2
  put i
  return $ (getIndentLevel i) ++ "(\n" ++ prettyE1 ++ "\n" ++ (getIndentLevel (i+1))  ++ "-\n" ++ prettyE2 ++ ")"
statePrettyPrint (Div e1 e2) = do
  i <- get
  put (i+1)
  prettyE1 <- statePrettyPrint e1
  prettyE2 <- statePrettyPrint e2
  put i
  return $ (getIndentLevel i) ++ "(\n" ++ prettyE1 ++ "\n" ++ (getIndentLevel (i+1))  ++ "/\n" ++ prettyE2 ++ ")"

prettyPrint :: Storage -> String
prettyPrint Storage {iState=i, expr=(Var c)} = (getIndentLevel i) ++ [c]
prettyPrint Storage {iState=i, expr=(N n)} = (getIndentLevel i) ++ show n
prettyPrint Storage {iState=i, expr=(Assign c e)} =
  (getIndentLevel i) ++ "<" ++ [c] ++ ":" ++ newLine ++ prettyExpr
    where prettyExpr = prettyPrint $ Storage {iState=iLevel, expr=e}
          iLevel = findIndent i e
          newLine = case iLevel of
            0 -> ""
            _ -> "\n"
prettyPrint Storage {iState=i, expr=(Add e1 e2)} =
  (getIndentLevel i) ++ "(\n" ++ prettyE1 ++ "\n" ++ (getIndentLevel (i+1))  ++ "+\n" ++ prettyE2 ++ ")"
  where prettyE1 = prettyPrint $ Storage {iState=(i+1), expr=e1}
        prettyE2 = prettyPrint $ Storage {iState=(i+1), expr=e2}
prettyPrint Storage {iState=i, expr=(Sub e1 e2)} =
  (getIndentLevel i) ++ "(\n" ++ prettyE1 ++ "\n" ++ (getIndentLevel (i+1)) ++ "-\n" ++ prettyE2 ++ ")"
  where prettyE1 = prettyPrint $ Storage {iState=(i+1), expr=e1}
        prettyE2 = prettyPrint $ Storage {iState=(i+1), expr=e2}  

findIndent :: Int -> Expr -> Int
findIndent _ (Var _) = 0
findIndent _ (N _) = 0
findIndent i _ = i+1
