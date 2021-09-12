module Main where

import Bootstrap
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))


