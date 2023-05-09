module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
--import Text.Parsec.Expr
--import Control.Monad (void)
import Language
-- LispVal data type from the previous example


-- Language definition for Parsec
lexer = Token.makeTokenParser emptyDef
  { Token.commentStart    = ""
  , Token.commentEnd      = ""
  , Token.commentLine     = ";"
  , Token.identStart      = letter <|> symbol
  , Token.identLetter     = letter <|> digit <|> symbol
  , Token.reservedNames   = []
  , Token.reservedOpNames = []
  }
  where
    symbol = oneOf "!$%&*+-/:<=>?@^_~"

parens = Token.parens lexer
identifier = Token.identifier lexer
stringLiteral = Token.stringLiteral lexer
integer = Token.integer lexer

parseAtom :: Parser LispVal
parseAtom = Atom <$> identifier

parseString :: Parser LispVal
parseString = String <$> stringLiteral

parseNumber :: Parser LispVal
parseNumber = Number <$> integer

parseBool :: Parser LispVal
parseBool = Bool <$> (char '#' *> (True <$ char 't' <|> False <$ char 'f'))

parseList :: Parser LispVal
parseList = List <$> parens (parseExpr `sepBy` spaces)
{-
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- parseExpr `sepBy` spaces
  tail <- char '.' *> spaces *> parseExpr
  return $ DottedList head tail
-}
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseBool
  <|> parseList
  -- <|> parens parseDottedList

-- Parse a Lisp expression from a string
readExpr :: String -> Either ParseError LispVal
readExpr = parse parseExpr "lisp"
