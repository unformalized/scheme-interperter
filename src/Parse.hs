module Parse where

import Text.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec hiding (spaces)
-- utils
import Control.Monad ( liftM )
import Control.Applicative ( liftA2 )
import Value ( LispVal(Number, Atom, Bool, String) )

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

many' :: ((Stream s m t), Monoid a) => ParsecT s u m a -> ParsecT s u m a
many' p = many1' p <|> pure mempty

many1' :: ((Stream s m t), Monoid a) => ParsecT s u m a -> ParsecT s u m a
many1' p = do { x <- p; xs <- many' p; return (x<>xs); }


parseBackslash :: Parser Char
parseBackslash = char '\\'

parseEscape :: Parser String
parseEscape =
    liftA2 (\c1 c2 -> [c1, c2]) parseBackslash (
        char '"' <|>
        char 'n' <|>
        char 'r' <|>
        char 't' <|>
        char '\\'
    )

parseString :: Parser LispVal
parseString = do
    char '"'
    s <- many (noneOf "\"")
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

