{-# LANGUAGE ScopedTypeVariables #-}
module Main where

   --import Tokenizer
   import Data.Char
   import Data.Map
   import Control.Monad.State
   import Data.Maybe
--   import StateEvalAndParse
   import MaybeEvalAndParse 
   import PrettyPrinter
   import DListPrettyPrinter
   import DList
   main :: IO ()
   main = do
      fileContent <- readFile "src/expression.txt"
      let parsed = evalState (parse (stripAllWhitespace fileContent)) ""
      let pretty = DListPrettyPrinter.statePrettyPrint parsed
      writeFile "src/out.txt" $ DList.toList (evalState pretty 0)
      out <- readFile "src/out.txt"
      let parsed2 = evalState (parse (stripAllWhitespace out)) ""
      let (e :: EvalSt (Maybe Int)) = evalM parsed2
      print $ evalState e []

   stripAllWhitespace :: String -> String
   stripAllWhitespace [] = []
   stripAllWhitespace (c : str) = if isSeparator c || c == '\n'
                                  then stripAllWhitespace str
                                  else c : (stripAllWhitespace str) 


