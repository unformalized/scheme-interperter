-- |
module Evaluation where

import Control.Monad.Error (MonadError (throwError))
import qualified Data.Vector as V
import Error (IOThrowsError, LispError (..), ThrowsError, liftThrows)
import PrimOp (primitives)
import Repl (Env, defineVar, getVar, setVar)
import Value (LispVal (..))

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
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)
