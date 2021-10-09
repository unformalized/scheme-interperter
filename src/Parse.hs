{-# LANGUAGE FlexibleContexts #-}
module Parse where

import Text.Parsec hiding (spaces, try)
import Text.ParserCombinators.Parsec ( Parser, try )
-- utils
import Control.Monad ( liftM )
import Control.Applicative ( liftA2, Alternative (empty) )
-- struct
import Value ( LispVal(Number, Atom, Bool, String) )

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseBackslash :: Parser Char
parseBackslash = char '\\'

parseEscape :: Parser Char
parseEscape = parseBackslash >> choice (zipWith escapedChar codes replacements)
  where
    escapedChar code replacement = char code >> return replacement
    codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ chars
                 char '"'
                 return $ String x
  where chars = parseEscape <|> noneOf "\""

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal 
parseNumber = liftM (Number . read) $ many1 digit

parseNumber' :: Parser LispVal
parseNumber' = many1 digit >>= (\num ->
    let lispN = Number (read num)
    in return lispN
  )


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

