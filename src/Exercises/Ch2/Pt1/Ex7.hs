-- Exercisee/Ch2/Pt1/Ex7.hs
module Ex7 where

-- Exercise 7
-- Add data types and parsers to support the full numeric tower of Scheme
-- numeric types. Haskell has built-in types to represent many of these; check
-- the Prelude. For others, you can define compound types that represent eg. a
-- `Rational` as a numerator and denominator, or a `Complex` as a real and an
-- imaginary part (each itself a `Real`).

import Data.Char                            (digitToInt, isSpace)
import Data.Complex
import Data.Ratio
import Control.Monad
import Numeric
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- (a b c ... . z)
             | Number LispNum
             | Character String
             | String String
             | Bool Bool
             deriving Show -- debug purposes

data LispNum = Complex (Complex Float)
             | Real Float
             | Rational (Ratio Integer)
             | Integer Integer
             deriving (Eq, Show)

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
parseNumber = do
  n <- (try parseBin) <|> (try parseOct) <|> (try parseHex) <|> (try parseFlt) 
    <|> (try parseRat) <|> (try parseCmp) <|> parseDec
  return $ Number n
  where
    parseBin = do
      string "#b"
      binStr <- many1 $ oneOf "01"
      -- cribbed from http://stackoverflow.com/a/26961027/1893155
      let binVal = foldl (\acc x -> acc * 2 + digitToInt x) 0 binStr
      return $ Integer (toInteger binVal)
    parseOct = do
      string "#o"
      octStr <- many1 octDigit
      let octVal = fst $ (readOct octStr) !! 0 
      return $ Integer octVal
    parseDec = parseDecNoPre <|> parseDecPre
    parseDecNoPre = do
      decStr <- many1 digit
      return $ (Integer . read) decStr
    parseDecPre = do
      string "#d"
      decStr <- many1 digit
      return $ (Integer . read) decStr
    parseHex = do
      string "#x"
      hexStr <- many1 hexDigit 
      let hexVal = fst $ (readHex hexStr) !! 0
      return $ Integer hexVal
    parseFlt = do
      whole <- many1 digit
      string "."
      mantissa <- many1 digit
      let fltVal = fst $ (readFloat (whole ++ "." ++ mantissa) !! 0)
      return $ Real $ fltVal
    parseRat = do
      m <- many1 digit
      many space
      string "/"
      many space
      n <- many1 digit
      let (m', n') = (fst $ (readDec m) !! 0, fst $ (readDec n) !! 0)
      return $ Rational $ m' % n'
    parseCmp = do
      (Real a) <- (try parseDec) <|> parseFlt
      many space
      string "+"
      many space
      (Real b) <- (try parseDec) <|> parseFlt
      char 'i'
      return $ Complex $ a :+ b

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

