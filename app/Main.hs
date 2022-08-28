module Main where

import Bootstrap (readExpr)
import Control.Monad (liftM)
import Error (extractValue, trapError)
import Evaluation (eval)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evalVal <- return $ liftM show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evalVal
