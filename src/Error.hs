{-# LANGUAGE InstanceSigs #-}

module Error where

import Control.Monad.Error (Error (..), ErrorT (runErrorT), MonadError (throwError), catchError)
import Data.Functor ((<&>))
import Text.ParserCombinators.Parsec (ParseError)
import Value (LispVal, unWordList)

data LispError
  = NumArgs Integer [LispVal]
  | ArgsError String [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (TypeMismatch expected found) = "Invalid type: " ++ expected ++ ", found: " ++ show found
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args, actual args: " ++ unWordList found
showError (ArgsError expected found) = "Expected " ++ show expected ++ " , actual args: " ++ unWordList found
showError (Parser parseErr) = "Parse error at: " ++ show parseErr
showError (Default error) = "Default error: " ++ show error

instance Show LispError where
  show :: LispError -> String
  show = showError

instance Error LispError where
  noMsg :: LispError
  noMsg = Default "An error has occurred"
  strMsg :: String -> LispError
  strMsg = Default

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = error (showError err)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) <&> extractValue
