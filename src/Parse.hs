module Parse where

import Text.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec hiding (spaces)
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

parseEscape :: Parser String
parseEscape =
    string "\\\n" <|>
    string "\\\t" <|>
    string "\\\\" <|>
    string "\\\r" <|>
    string "\\\""

handleEscape :: Parser String
handleEscape = do
    normal <- many (noneOf "\\\"")
    case normal of
        [] -> return []
        xs -> do
            escape <- parseEscape <|> return []
            rest <- handleEscape
            return (normal ++ escape ++ rest)

parseString :: Parser LispVal
parseString = do
    char '"'
    -- s <- many' (many (noneOf "\"") <|> parseEscape)
    s <- handleEscape
    char '"'
    return $ String s

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

