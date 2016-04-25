-- Exercises/Ch2/Pt1/Ex6.hs
module Ex6 where

-- Exercise 6
-- Add a `Float` constructor to `LispVal`, and create a parser for character
-- literals as described in R5RS.

import Data.Char                            (digitToInt, isSpace)
import Control.Monad
import Numeric
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- (a b c ... . z)
             | Number Integer
             | Float Float -- could use Double 
             | Character String
             | String String
             | Bool Bool
             deriving Show -- debug purposes

parseCharacter :: Parser LispVal
parseCharacter = do
  string "#\\"
  character <- many $ satisfy $ not . isSpace
  return $ Character character
  

parseString :: Parser LispVal
parseString = do
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

parseBool :: Parser LispVal
parseBool = do
  try (char '#')
  v <- oneOf "tf"
  return $ case v of
    't' -> Bool True
    'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              return $ Atom (first:rest)

parseNumber :: Parser LispVal
parseNumber = (try parseBin) <|> (try parseOct) <|> (try parseHex) <|> (try parseFlt) <|> parseDec
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
    parseFlt = do
      whole <- many1 digit
      string "."
      part <- many1 digit
      let fltVal = fst $ (readFloat (whole ++ "." ++ part) !! 0)
      return $ Float $ fltVal

-- TODO : I don't like all these `try`s -- better way to do this?
parseExpr :: Parser LispVal
parseExpr = (try parseNumber)
         <|> (try parseString)
         <|> (try parseCharacter)
         <|> (try parseBool)
         <|> (try parseAtom)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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

