module Lib
    ( someFunc
    ) where
import Parser
import Semantics.CEK

someFunc :: IO ()
someFunc = do
  let input = "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))"
  putStrLn $ "Input: " ++ input
  case readExpr input of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> putStrLn $ "Parsed: " ++ show val
