-- Exercise2.hs
module Exercise2 where

-- Exercise 2.
-- Add support for vectors. The Haskell representation is up to you: GHC does
-- have an `Array` type, but it can be difficult to use. Strictly speaking, a
-- vector should have constant-time indexing and updating, but destructive
-- update in a purely functional language is difficult. You may have a better
-- idea how to do this after hte section on `set!`, later in this tutorial.

import Control.Monad
import Data.Array
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- (a b c ... . z)
             | Vector (Array Int LispVal)
             | Number Integer
             | String String
             | Bool Bool

-- Begin helper functions (functions which don't return `LispVal`s):

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- End helper functions.

-- Begin Lisp parsing functions:

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"") -- 0 or more non-doublequote characters.
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- End Lisp parsing functions.

-- Start recursive parsers:

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  hd <- endBy parseExpr spaces
  tl <- char '.' >> spaces >> parseExpr
  return $ DottedList hd tl

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- TODO these could be refactored into a single fn.
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseAtUnQuote :: Parser LispVal
parseAtUnQuote = do
  string ",@"
  x <- parseExpr
  return $ List [Atom "at-unquote", x]

parseVector :: Parser LispVal
parseVector = do
  char '#'
  char '('
  (List v) <- parseList
  char ')'
  return $ Vector $ listArray (0, length v) v

-- End recursive parsers.

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseVector
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> (try parseUnQuote)
         <|> parseAtUnQuote
         -- TODO maybe refactor this into a general parseAnyList?
         -- I just don't like the do and char actions included amidst the
         -- self-contained parse[Atom/String/Number/Quoted] functions.
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

-- Begin "main"/"driver" functions:

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

-- End "main"/"driver" functions.

