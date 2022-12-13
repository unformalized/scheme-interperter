{-# LANGUAGE InstanceSigs #-}

-- |
module Value where

import Control.Monad.Error (Error (..), ErrorT (runErrorT), MonadError (catchError, throwError))
import Data.Functor ((<&>))
import Data.IORef (IORef)
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec (ParseError)

-- data Number = Integer | Double

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Char Char
  | Float Float
  | Vector (V.Vector LispVal)
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], varArgs :: Maybe String, body :: [LispVal], closure :: Env}

instance Show LispVal where
  show :: LispVal -> String
  show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float f) = show f
showVal (Char c) = show c
showVal (List contents) = "(" ++ unWordList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unWordList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector xs) = "#(" ++ unWordVector xs ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, varArgs = varArgs, body = body, closure = env} =
  "lambda (" ++ unwords (map show args)
    ++ ( case varArgs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"

unWordList :: [LispVal] -> String
unWordList = unwords . map showVal

unWordVector :: V.Vector LispVal -> String
unWordVector xs = V.foldl1 (\x1 x2 -> x1 ++ " " ++ x2) $ V.map showVal xs

-- env

type Env = IORef [(String, IORef LispVal)]

-- error

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

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = error (showError err)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) <&> extractValue
