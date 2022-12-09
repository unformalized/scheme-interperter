{-# LANGUAGE InstanceSigs #-}

-- |
module Value (LispVal (..), unWordList, unWordVector) where

import qualified Data.Vector as V

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

unWordList :: [LispVal] -> String
unWordList = unwords . map showVal

unWordVector :: V.Vector LispVal -> String
unWordVector xs = V.foldl1 (\x1 x2 -> x1 ++ " " ++ x2) $ V.map showVal xs
