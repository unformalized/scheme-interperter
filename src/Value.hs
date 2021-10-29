-- |

module Value where

data Number = Integer | Double

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Float Float
             | Bool Bool deriving Show
