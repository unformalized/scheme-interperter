module VectorSpec (
  test
  ) where

import GHC.Prim
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive

import Data.Vector.Unboxed.Mutable hiding (forM_)
import Data.Vector.Unboxed (freeze)

import qualified Data.Vector.Unboxed.Base as V


testVector :: PrimMonad m => m (V.Vector Int)
testVector = do
  v <- new 10
  forM_ [0..9] $ \i ->
    write v i (2 * i)
  freeze v

vecIO :: IO (V.Vector Int)
vecIO = testVector

vecST :: ST s (V.Vector Int)
vecST = testVector

test :: IO ()
test = do
  vecIO >>= print
  print $ runST vecST
