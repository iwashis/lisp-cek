module Language where

-- Algebraic datatype representation of Lisp expressions
data LispVal
  = Atom String
  | List [LispVal]
  | Number Integer
  | String String
  | Bool Bool
  deriving (Eq)


instance Show LispVal where
  show (Atom name) = name
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (Number num) = show num
  show (String str) = "\"" ++ str ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show