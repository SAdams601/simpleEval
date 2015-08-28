module DListPrettyPrinter where
import StateEvalAndParse
import Control.Monad.State
import DList
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

statePrettyPrint :: Expr -> PrintState (DList Char)
statePrettyPrint (Var c) = do
  i <- get
  return $ append (fromList (getIndentLevel i)) (fromList [c])
statePrettyPrint (N n) = do
  i <- get
  return $ append (fromList (getIndentLevel i)) (fromList $ show n)
statePrettyPrint (Neg e) = do
  i <- get
  prettyExpr <- statePrettyPrint e
  return $ append (fromList (getIndentLevel i)) (append (fromList "-") prettyExpr)
statePrettyPrint (Assign c e) = do
  i <- get
  let iLevel = findIndent i e
      newLine = case iLevel of
        0 -> ""
        _ -> "\n"
  put iLevel
  prettyExpr <- statePrettyPrint e
  put i
  return $ append (appendMany [(getIndentLevel i), "<", [c], ":", newLine]) prettyExpr
statePrettyPrint (Add e1 e2) = do
  i <- get
  put (i+1)
  se1 <- statePrettyPrint e1
  se2 <- statePrettyPrint e2
  let prettyE1 = toList se1
      prettyE2 = toList se2
  put i
  return $ appendMany [(getIndentLevel i), "(\n", prettyE1, "\n", (getIndentLevel (i+1)), "+\n", prettyE2, ")"]
statePrettyPrint (Sub e1 e2) = do
  i <- get
  put (i+1)
  se1 <- statePrettyPrint e1
  se2 <- statePrettyPrint e2
  let prettyE1 = toList se1
      prettyE2 = toList se2
  put i
  return $ appendMany [(getIndentLevel i), "(\n", prettyE1, "\n", (getIndentLevel (i+1)), "-\n", prettyE2, ")"]
statePrettyPrint (Div e1 e2) = do
  i <- get
  put (i+1)
  se1 <- statePrettyPrint e1
  se2 <- statePrettyPrint e2
  let prettyE1 = toList se1
      prettyE2 = toList se2
  put i
  return $ appendMany [(getIndentLevel i), "(\n", prettyE1, "\n", (getIndentLevel (i+1)), "/\n", prettyE2, ")"]

prettyPrint :: Storage -> DList Char
prettyPrint Storage {iState=i, expr=(Var c)} = append (fromList (getIndentLevel i)) (fromList [c])
prettyPrint Storage {iState=i, expr=(N n)} = append (fromList (getIndentLevel i)) (fromList $ show n)
prettyPrint Storage {iState=i, expr=(Assign c e)} =
  append (appendMany [(getIndentLevel i), "<", [c], ":", newLine]) prettyExpr
    where prettyExpr = prettyPrint $ Storage {iState=iLevel, expr=e}
          iLevel = findIndent i e
          newLine = case iLevel of
            0 -> ""
            _ -> "\n"
prettyPrint Storage {iState=i, expr=(Add e1 e2)} =
  appendMany [(getIndentLevel i), "(\n", prettyE1, "\n", (getIndentLevel (i+1)), "+\n", prettyE2, ")"]
  where prettyE1 = toList $ prettyPrint $ Storage {iState=(i+1), expr=e1}
        prettyE2 = toList $ prettyPrint $ Storage {iState=(i+1), expr=e2}
prettyPrint Storage {iState=i, expr=(Sub e1 e2)} =
  appendMany [(getIndentLevel i), "(\n", prettyE1, "\n", (getIndentLevel (i+1)), "-\n", prettyE2, ")"]
  where prettyE1 = toList $ prettyPrint $ Storage {iState=(i+1), expr=e1}
        prettyE2 = toList $ prettyPrint $ Storage {iState=(i+1), expr=e2}  

findIndent :: Int -> Expr -> Int
findIndent _ (Var _) = 0
findIndent _ (N _) = 0
findIndent i _ = i+1
