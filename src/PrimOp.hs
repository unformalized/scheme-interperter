-- |
module PrimOp where

import Control.Monad.Error (MonadError (throwError))
import Error (LispError (..), ThrowsError)
import Value (LispVal (..))

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop (+)),
    ("mod", numericBinop (-)),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]
    ++ map (\op -> (op, typeBinop op)) typeOpList
    ++ symbolOpList

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- unpackNum (String s) =
--   let parsed = reads s
--    in if null parsed
--         then 0
--         else fst $ head parsed
-- unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

typeOpList :: [String]
typeOpList = ["string?", "boolean?", "number?", "symbol?"]

typeBinop :: String -> [LispVal] -> ThrowsError LispVal
typeBinop "string?" ((String _) : _) = return $ Bool True
typeBinop "string?" _ = return $ Bool False
typeBinop "boolean?" ((Bool _) : _) = return $ Bool True
typeBinop "boolean?" _ = return $ Bool False
typeBinop "number?" ((Number _) : _) = return $ Bool True
typeBinop "number?" _ = return $ Bool False
typeBinop "symbol?" ((Atom _) : _) = return $ Bool True
typeBinop "symbol?" _ = return $ Bool False
typeBinop typeFunc _ = throwError $ NotFunction "unrecognized primitive function" typeFunc

-- symbol operator

symbolOpList :: [(String, [LispVal] -> ThrowsError LispVal)]
symbolOpList =
  [ ("symbol->string", symbol2str),
    ("string->symbol", str2symbol)
  ]

symbol2str :: [LispVal] -> ThrowsError LispVal
symbol2str [Atom name] = return $ String name
symbol2str [val] = throwError $ ArgsError "symbol" [val]
symbol2str args = throwError $ NumArgs 1 args

str2symbol :: [LispVal] -> ThrowsError LispVal
str2symbol [String name] = return $ Atom name
str2symbol [val] = throwError $ ArgsError "string" [val]
str2symbol args = throwError $ NumArgs 1 args
