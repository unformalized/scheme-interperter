-- |
module PrimOp where

import Control.Monad.Error (MonadError (throwError))
import Data.Functor ((<&>))
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
    ("remainder", numericBinop rem),
    ("=", numBoolBinOp (==)),
    ("<", numBoolBinOp (<)),
    (">", numBoolBinOp (>)),
    ("/=", numBoolBinOp (/=)),
    (">=", numBoolBinOp (>=)),
    ("<=", numBoolBinOp (<=)),
    ("&&", boolBoolBinOp (&&)),
    ("||", boolBoolBinOp (||)),
    ("string=?", strBoolBinOp (==)),
    ("string?", strBoolBinOp (>)),
    ("string<=?", strBoolBinOp (<=)),
    ("string>=?", strBoolBinOp (>=))
  ]
    ++ map (\op -> (op, typeBinop op)) typeOpList
    ++ symbolOpList

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params <&> Number . foldl1 op

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinOp unpackNum

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
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

-- List operator

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xLast] = return $ DottedList (x : xs) xLast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList
