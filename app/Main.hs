module Main where

import Bootstrap (readExpr)
import Control.Monad (liftM)
import Error (extractValue, trapError)
import Evaluation (eval)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let evalVal = liftM show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evalVal
