-- |
module Evaluation where

import Control.Monad (liftM)
import Control.Monad.Error (MonadError (throwError), MonadIO (liftIO))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import Parse (parseExpr, spaces)
import Repl (bindVars, defineVar, getVar, setVar)
import System.IO (IOMode, hGetLine, hPrint, stdin, stdout)
import Text.ParserCombinators.Parsec (Parser, endBy, parse)
import Value (Env, IOThrowsError, LispError (..), LispVal (..), ThrowsError, liftThrows, makeNormalFunc, makeVarArgs)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      Bool True -> eval env conseq
      err -> throwError $ TypeMismatch "bool" err
eval env (List [Atom "set!", Atom var, from]) =
  eval env from >>= setVar env var
eval env (List [Atom "define", Atom var, from]) =
  eval env from >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varArgs : body)) =
  makeVarArgs varArgs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varArgs : body)) =
  makeVarArgs varArgs env params body
eval env (List (Atom "lambda" : varArgs@(Atom _) : body)) =
  makeVarArgs varArgs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (func : args)) = do
  func <- eval env func
  argValues <- mapM (eval env) args
  apply func argValues
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varArgs body closure) args =
  if num params /= num args && isNothing varArgs
    then throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varArgs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
apply func _ = throwError $ NotFunction "Unrecognized primitive function args" $ show func

-- read expression

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
  case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

-- io functions

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc badArgList = throwError $ NumArgs 2 badArgList

-- wraps the hGetLine
readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc badArgList = throwError $ ArgsError "LispVal Port" badArgList

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)
writeProc badArgList = throwError $ ArgsError "lispValue" badArgList

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents badArgList = throwError $ ArgsError "LispVal String as filename" badArgList

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll badArgList = throwError $ ArgsError "LispVal String as filename" badArgList
