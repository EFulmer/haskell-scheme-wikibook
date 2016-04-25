-- Chapter2Exercise2.hs
module Chapter2Exercise2 where

-- Exercise 2.
-- Our strings aren't quite R5RS compliant, because they don't support escaping
-- of internal quotes within the string. Change `parseString` so that `\"` gives a
-- literal quote character instead of terminating the string. You may want to
-- replace `noneOf "\""` with a new parser action that accepts *either* a
-- non-quote character *or* a backslash followed by a quote mark.

import Control.Monad
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- (a b c ... . z)
             | Number Integer
             | String String
             | Bool Bool
             deriving Show -- added for debug purposes!

parseString' :: Parser LispVal
parseString' = do
  char '"'
  -- order is important here; need to try parsing an escaped quote first because
  -- otherwise we fail when we reach the quotation character of the escape
  -- sequence
  x <- many $ choice [escQuote, nonQuote]
  char '"'
  return $ String x
  where
    nonQuote = noneOf "\""
    escQuote = char '\\' >> char '\"'

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

