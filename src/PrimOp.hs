-- |
module PrimOp where

import Value (LispVal (..))

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop (+)),
    ("mod", numericBinop (-)),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]
    ++ map (\op -> (op, typeBinop op)) typeOpList
    ++ symbolOpList

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- unpackNum (String s) =
--   let parsed = reads s
--    in if null parsed
--         then 0
--         else fst $ head parsed
-- unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

typeOpList :: [String]
typeOpList = ["string?", "boolean?", "number?", "symbol?"]

typeBinop :: String -> [LispVal] -> LispVal
typeBinop "string?" ((String _) : _) = Bool True
typeBinop "string?" _ = Bool False
typeBinop "boolean?" ((Bool _) : _) = Bool True
typeBinop "boolean?" _ = Bool False
typeBinop "number?" ((Number _) : _) = Bool True
typeBinop "number?" _ = Bool False
typeBinop "symbol?" ((Atom _) : _) = Bool True
typeBinop "symbol?" _ = Bool False
typeBinop _ _ = Bool False

-- symbol operator

symbolOpList :: [(String, [LispVal] -> LispVal)]
symbolOpList =
  [ ("symbol->string", symbol2str . head),
    ("string->symbol", str2symbol . head)
  ]

symbol2str :: LispVal -> LispVal
symbol2str (Atom name) = String name
symbol2str _ = String ""

str2symbol :: LispVal -> LispVal
str2symbol (String name) = Atom name
str2symbol _ = Atom ""
