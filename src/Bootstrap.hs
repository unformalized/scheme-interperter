module Bootstrap
  ( readExpr,
  )
where

import Control.Monad.Error (MonadError (throwError))
import Parse (parseExpr, spaces)
import Text.ParserCombinators.Parsec (parse)
import Value (LispError (..), LispVal (String), ThrowsError)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
