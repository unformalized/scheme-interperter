module Bootstrap
  ( readExpr,
  )
where

import Parse (parseExpr, spaces)
import Text.ParserCombinators.Parsec (parse)
import Value (LispVal (String))

readExpr :: String -> LispVal
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val
