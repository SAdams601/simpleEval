module Main where
   --import Tokenizer
   import Data.Char
   import Data.Map
   import Control.Monad.State
   import Data.Maybe
   import StateEvalAndParse
   import PrettyPrinter

   main :: IO ()
   main = do
      fileContent <- readFile "src/prettyExpression.txt"
      let parsed = evalState (parse (stripAllWhitespace fileContent)) ""
      let pretty = statePrettyPrint parsed
      writeFile "src/out.txt" $ evalState pretty 0
      out <- readFile "src/out.txt"
      let parsed2 = evalState (parse (stripAllWhitespace out)) ""
      let e = eval parsed2
      print $ evalState e []
      --let evaluation = eval parsed
      --print $ evalState evaluation []

   stripAllWhitespace :: String -> String
   stripAllWhitespace [] = []
   stripAllWhitespace (c : str) = if isSeparator c || c == '\n'
                                  then stripAllWhitespace str
                                  else c : (stripAllWhitespace str) 
{-}
      fileContent <- readFile "src/expression.txt"--filePath
      let ts = tokenize $ stripAllWhitespace fileContent
      let pst = ParsSt {tokens = ts, variables = empty}
      
      result <- (simpleEval pst)
      print result

   --simpleEval :: ParseInfo -> Int
   simpleEval ParsSt {tokens = [], variables = _ } = return 0 
   simpleEval pfo = do   
      --print pfo
      let (val, newPfo) = consumeToken pfo
      --print val
      otherVal <- simpleEval newPfo
      return $ val + otherVal

   consumeToken :: ParseInfo -> (Int, ParseInfo)
   consumeToken pst = let currToken = head $ tokens pst in
      case currToken of
            Add -> handleAdd pst
            Sub -> let (i,p) = handleAdd pst in
                     (-i,p)
            Name _ -> handleVar pst


   handleVar :: ParseInfo -> (Int,ParseInfo)
   handleVar pst = let name = head $ tokens pst in
      if (length $ tokens pst) < 2
         then (fromJust $ Data.Map.lookup (nm name) (variables pst), ParsSt {tokens = [], variables = variables pst})
         else 
            let next = head $ tail $ tokens pst
                rest = tail $ tail $ tokens pst in
            case next of
               Assign -> let val = v $ head rest in
                  (val, ParsSt {tokens = tail rest, variables = insert (nm name) val (variables pst)})
               _ -> (fromJust $ Data.Map.lookup (nm name) (variables pst), ParsSt {tokens = rest, variables = variables pst})



   handleAdd :: ParseInfo -> (Int, ParseInfo)
   handleAdd pst = let firstToken = head $ tail $ tokens pst 
                       rst = tail $ tail $ tokens pst in
      case firstToken of
         Value x -> (x, ParsSt {tokens = rst, variables = variables pst})
         Name _ -> handleVar $ ParsSt {tokens = tail $ tokens pst,  variables = variables pst}
   {-(v nxtToken, ParsSt {tokens = tail rst, variables = variables pst})
      where 
            rst = tail $ tokens pst
            nxtToken = head rst-}
             
   data ParseInfo = ParsSt
         {  tokens   :: [Token],
            variables :: Map String Int} deriving (Show)

   type ParseState = State ParseInfo-}

