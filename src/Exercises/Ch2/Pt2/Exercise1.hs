-- Exercise1.hs
module Exercise1 where

-- Exercise 1.
-- Add support for the backquote syntactic sugar: the Scheme standard details
-- what it should expand into (quasiquote/unquote).

import Control.Monad
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- (a b c ... . z)
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

-- TODO this and parseQuoted should probably be refactored into a single fn.
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = (char ',') >> parseExpr >>= (\x -> return $ List [Atom "quasiquote", x])


-- End recursive parsers.

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseQuasiQuoted
         -- TODO maybe refactor this into a general parseAnyList?
         -- I just don't like the ro and char actions included amidst the
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

