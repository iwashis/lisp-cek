module Semantics.CEK where

import Language

import qualified Data.Map as Map


data Closure = Closure [String] LispVal Environment
  deriving Show

type Control = Either LispVal Closure
type Environment = Map.Map String LispVal
type Kontinuation = [Frame]

data Frame
  = Apply [LispVal] Environment
  | Argument LispVal Environment
  | IfThenElse LispVal LispVal Environment
  deriving Show

data Machine = Machine Control Environment Kontinuation

evalCEK :: LispVal -> Either String LispVal
evalCEK e = runMachine (Machine (Left e) Map.empty [])

runMachine :: Machine -> Either String LispVal
runMachine (Machine (Left (Number n)) _ []) = Right (Number n)
runMachine (Machine (Left (String s)) _ []) = Right (String s)
runMachine (Machine (Left (Bool b)) _ []) = Right (Bool b)
runMachine m = step m >>= runMachine

step :: Machine -> Either String Machine
step (Machine (Left (Atom v)) env k) = case Map.lookup v env of
  Nothing -> Left $ "Unbound variable: " ++ v
  Just val -> Right $ Machine (Left val) env k

step (Machine (Left (List (Atom "quote" : [q]))) env k) = Right $ Machine (Left q) env k

step (Machine (Left (List (Atom "if" : [cond, trueExpr, falseExpr]))) env k) =
  Right $ Machine (Left cond) env (IfThenElse trueExpr falseExpr env : k)

step (Machine (Left (Bool b)) env (IfThenElse trueExpr falseExpr _ : k)) =
  Right $ Machine (Left (if b then trueExpr else falseExpr)) env k

step (Machine (Left (List [Atom "lambda", List args, body])) env k) =
  let argNames = map (\(Atom s) -> s) args
  in Right $ Machine (Right (Closure argNames body env)) env k

step (Machine (Left (List (fun : args))) env k) = Right $ Machine (Left fun) env (Apply args env : k)

step (Machine (Right (Closure params body clo)) env (Apply args callEnv : k)) =
  Right $ Machine (Left body) (Map.union (Map.fromList (zip params args)) clo) k

step (Machine (Left v) env (Argument expr callEnv : k)) = Right $ Machine (Left expr) callEnv (Apply [v] env : k)

step (Machine (Left v) env (Apply vs callEnv : k)) = Right $ Machine (Left (List $ reverse (v : vs))) callEnv k

step _ = Left "Invalid expression"

eval :: LispVal -> Either String LispVal
eval expr = step (Machine (Left expr) Map.empty []) >>= runMachine
