{-# LANGUAGE FlexibleContexts #-}

module Parse where

-- utils

import Control.Applicative (Alternative (empty), liftA2)
import Control.Monad (liftM)
-- struct

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Data.Char (isPrint)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Lib (ioListToVector, listToVector)
import Numeric
import Text.Parsec hiding (spaces, try)
import Text.ParserCombinators.Parsec (Parser, try)
import Value (LispVal (Atom, Bool, Char, DottedList, Float, List, Number, String, Vector))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany space

parseBackslash :: Parser Char
parseBackslash = char '\\'

parseEscape :: Parser Char
parseEscape = parseBackslash >> choice (zipWith escapedChar codes replacements)
  where
    escapedChar code replacement = char code >> return replacement
    codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ chars
  char '"'
  return $ String x
  where
    chars = parseEscape <|> noneOf "\""

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseDecNum :: Parser LispVal
parseDecNum = do
  decS <- many1 digit
  let decN : _ = readDec decS
  return . Number . fst $ decN

parseOctNum :: Parser LispVal
parseOctNum = do
  octS <- string "#o" >> many1 (oneOf octNS)
  case readOct octS of
    [] -> parserFail ("oct parse error: " ++ octS)
    octN : _ -> return . Number . fst $ octN
  where
    octNS = "01234567"

parseHexNum :: Parser LispVal
parseHexNum = do
  hexS <- string "#x" >> many1 (oneOf hexNS)
  case readHex hexS of
    [] -> parserFail ("hex parse error: " ++ hexS)
    hexN : _ -> return . Number . fst $ hexN
  where
    hexNS = "abcdef0123456789"

parseInteger :: Parser LispVal
parseInteger = choice [try parseDecNum, try parseOctNum, try parseHexNum]

parseChar :: Parser LispVal
parseChar = do
  c <- choice [try charNewline, try charSpace, try charPrint]
  return (Char c)
  where
    charSpace :: Parser Char
    charSpace = string "#\\space" >> return ' '
    charNewline :: Parser Char
    charNewline = string "#\\newline" >> return '\n'
    charPrint :: Parser Char
    charPrint = string "#\\" >> satisfy isPrint

parseFloat :: Parser LispVal
parseFloat = do
  integer <- many digit
  dot <- char '.'
  frac <- many digit
  let floatS = integer ++ (dot : frac)
      floatList = readFloat floatS
  case floatList of
    [] -> parserFail ("float parse error: " ++ floatS)
    (float : _) -> return . Float . fst $ float

parseExpr :: Parser LispVal
parseExpr =
  choice
    [ try parseInteger,
      try parseChar,
      try parseFloat,
      try parseString,
      try parseQuoted,
      try parseLispList,
      try parseLispVector,
      try parseAtom
    ]

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseLispList :: Parser LispVal
parseLispList = do
  char '('
  els <- try parseList <|> parseDottedList
  char ')'
  return els

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char '`'
  -- comma evaluate and comma at-sign (@) parse unfinished
  xs <- parseLispList
  return $ List [Atom "quasiquote", xs]

parseVector :: Parser LispVal
parseVector = liftM (Vector . V.fromList) $ sepBy parseExpr spaces

parseLispVector :: Parser LispVal
parseLispVector = do
  char '#'
  char '('
  els <- parseVector
  char ')'
  return els
