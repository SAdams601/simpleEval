module Tokenizer where
import Data.Char

data Token = Assign | Add | Sub | Value Int | Name String deriving (Show, Eq)

-- Assumes all whitespace has been removed
tokenize :: String -> [Token]
tokenize "" = []
tokenize s = (constructToken tokenStr) : tokenize rest
               where (tokenStr, rest) = chopToken s

v :: Token -> Int
v (Value x) = x

nm :: Token -> String
nm (Name s) = s

constructToken :: String -> Token
constructToken ":" = Assign
constructToken "+" = Add
constructToken "-" = Sub
constructToken s@(x : _)
   | isDigit x = Value (read s)
   | otherwise = Name s

specialChars :: [Char]
specialChars = [':', '+', '-']

chopToken :: String -> (String, String)
chopToken s@(c : rest) = if c `elem` specialChars
                         then ([c], rest)
                         else (name,afterValue)
                         where name = getValue s
                               afterValue = drop (length name) s



getValue :: String -> String
-- We are assuming that the expression is well formed so the first case is ok
getValue (c : []) = [c]
getValue (c : rest) = if c `elem` specialChars
                       then ""
                       else c : getValue rest

