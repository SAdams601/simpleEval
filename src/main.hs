module Main where
   import Tokenizer

   main = print "Hello, World!"


   parse :: [Token] -> Expr
   parse (t : ts) = 

   type Value = Int
   type Name = String

   data Expr =
      Add Expr Expr
    | Sub Expr Expr
    | Assign Name Value
    | Concrete Value  