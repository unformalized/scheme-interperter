{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( listToVector,
      ioListToVector
    ) where

import Value
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive ( PrimMonad (primitive) )
import Control.Monad.Trans.Maybe ( MaybeT )
import Text.ParserCombinators.Parsec ( Parser )
import Control.Monad.ST (ST)

listToVector :: (PrimMonad m) => LispVal -> m (Maybe (V.Vector LispVal))
listToVector val =
  case val of
    List xs -> do
      let len = length xs
      v <- VM.new len
      forM_ [0..(len - 1)] $ \i ->
        VM.write v i (xs !! i)
      fv <- V.freeze v
      return (Just fv)
    _ -> return Nothing
      
ioListToVector :: LispVal -> ST s (Maybe (V.Vector LispVal))
ioListToVector = listToVector
