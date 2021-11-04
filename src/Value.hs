-- |

module Value where

import Data.Vector (Vector)

data Number = Integer | Double

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Float Float
             | Vector (Vector LispVal)
             | Bool Bool deriving Show
