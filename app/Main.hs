module Main where

import Bootstrap (readExpr)
import Evaluation (eval)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn (show $ eval $ readExpr (head args))
