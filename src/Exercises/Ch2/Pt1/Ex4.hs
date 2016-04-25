-- Chapter2Exercise4.hs
module Chapter2Exercise4 where

-- Exercise 4.
-- Change parseNumber to support the Scheme standard for different bases. You
-- may find the readOct and readHex functions useful.

import Data.Char (digitToInt)
import Control.Monad
import Numeric
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

parseNumber''' :: Parser LispVal
parseNumber''' = (try parseBin) <|> (try parseOct) <|> (try parseHex) <|> parseDec
  where
    parseBin = do
      string "#b"
      binStr <- many1 $ oneOf "01"
      -- cribbed from http://stackoverflow.com/a/26961027/1893155
      let binVal = foldl (\acc x -> acc * 2 + digitToInt x) 0 binStr
      return $ Number (toInteger binVal)
    parseOct = do
      string "#o"
      octStr <- many1 octDigit
      let octVal = fst $ (readOct octStr) !! 0 
      return $ Number octVal
    parseDec = parseDecNoPre <|> parseDecPre
    parseDecNoPre = do
      decStr <- many1 digit
      return $ (Number . read) decStr
    parseDecPre = do
      string "#d"
      decStr <- many1 digit
      return $ (Number . read) decStr
    parseHex = do
      string "#x"
      hexStr <- many1 hexDigit 
      let hexVal = fst $ (readHex hexStr) !! 0
      return $ Number hexVal
      
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

