module Main where

import Bootstrap (readExpr)
import Control.Monad (liftM)
import Error (extractValue, trapError)
import Evaluation (eval)
import Repl (readPrompt)
import System.Environment (getArgs)

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"
