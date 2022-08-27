-- |
module Evaluation where

import qualified Data.Vector as V
import Value (LispVal (..))

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float f) = show f
showVal (Char c) = show c
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector xs) = "#(" ++ unwordsVector xs ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unwordsVector :: V.Vector LispVal -> String
unwordsVector xs = V.foldl1 (\x1 x2 -> x1 ++ " " ++ x2) $ V.map showVal xs
