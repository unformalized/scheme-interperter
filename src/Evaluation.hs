-- |
module Evaluation where

import qualified Data.Vector as V
import Value (LispVal (..))

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
