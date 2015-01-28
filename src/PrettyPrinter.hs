module PrettyPrinter where
import StateEvalAndParse
import Control.Monad.State

data Storage = Storage {
  iState :: Int,
  expr :: Expr
  
                                 } deriving Show
type PrintState = State Storage

indent = "   "

getIndentLevel :: Int -> String
getIndentLevel l = foldr (++) "" $ take l $ repeat indent
{-

-}
prettyPrependPrint :: Storage -> String
prettyPrependPrint Storage {iState=i, expr=(Var c)} = (getIndentLevel i) ++ [c]
prettyPrependPrint Storage {iState=i, expr=(N n)} = (getIndentLevel i) ++ show n
prettyPrependPrint Storage {iState=i, expr=(Assign c e)} =
  (getIndentLevel i) ++ "(" ++ "<" ++ [c] ++ ":" ++ prettyExpr ++ ")"
    where prettyExpr = prettyPrependPrint $ Storage {iState=iLevel, expr=e}
          iLevel = findIndent i e
prettyPrependPrint Storage {iState=i, expr=(Add e1 e2)} =
  (getIndentLevel i) ++ "(" ++ prettyE1 ++ "+\n" ++ prettyE2 ++ ")"
  where prettyE1 = prettyPrependPrint $ Storage {iState=(i+1), expr=e1}
        prettyE2 = prettyPrependPrint $ Storage {iState=(i+1), expr=e2}
prettyPrependPrint Storage {iState=i, expr=(Sub e1 e2)} =
  (getIndentLevel i) ++ "(\n" ++ prettyE1 ++ "-\n" ++ prettyE2 ++ ")"
  where prettyE1 = prettyPrependPrint $ Storage {iState=(i+1), expr=e1}
        prettyE2 = prettyPrependPrint $ Storage {iState=(i+1), expr=e2}  

findIndent :: Int -> Expr -> Int
findIndent _ (Var _) = 0
findIndent _ (N _) = 0
findIndent i _ = i+1
