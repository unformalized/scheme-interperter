module Bootstrap
  ( readExpr,
  )
where

import Control.Monad.Error (MonadError (throwError))
import Error (LispError (..), ThrowsError)
import Parse (parseExpr, spaces)
import Text.ParserCombinators.Parsec (parse)
import Value (LispVal (String))

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
