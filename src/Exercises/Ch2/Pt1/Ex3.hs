-- Chapter2Exercise3.hs
module Chapter2Exercise3 where

-- Exercise 3.
-- Modify the previous exercise to support `\n`, `\r`, `\t`, `\\`, and any other
-- desired escape characters

import Control.Monad
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- (a b c ... . z)
             | Number Integer
             | String String
             | Bool Bool
             deriving Show -- debug purposes

parseString' :: Parser LispVal
parseString' = do
  char '"'
  -- order is important here; need to try parsing an escape sequence first because
  -- otherwise we fail when we reach the second character of the escape
  -- sequence
  x <- many $ choice $ escChars ++ [nonQuote]
  char '"'
  return $ String x
  where
    nonQuote = noneOf "\""
    -- taken from here:
    -- https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
    -- (excluded characters by oct/hex codes)
    escChars = [ char '\\' >> char x | x <- "abfnrtv\\'\"?" ]

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

parseNumber' :: Parser LispVal
parseNumber' = do
                digits <- many1 digit
                return $ (Number . read) digits

parseNumber'' :: Parser LispVal
parseNumber'' = (many1 digit) >>= (\x -> return $ (Number . read) x)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString'
         <|> parseNumber'

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

