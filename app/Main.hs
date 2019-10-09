module Main where

import Data.Aeson hiding ((.:))
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.Async
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa
import Data.Array.Repa.Eval (fromList)
import Debug.Trace
import qualified Data.Vector.Storable as V
import SKLearn.PyInterOp
import SKLearn.Classes
import SKLearn.ExtraTrees


main :: IO ()
main = do
  runInterpreter
  -- res <- takeMVar mvarOut
  -- print res

  et@(ExtraTreesRegressor etPtr) <-
    new defaultExtraTreesRegressorParams :: IO (ExtraTreesRegressor)
  let x = fromList (ix2 2 2) [1::Double,2,3,4]
      y = fromList (ix1 2) [5::Double,3]
  fitS et x y

  traceIO "to numpy..."
  npX <- repaToNumpy x
  traceIO "to repa..."
  r' <- numpyToRepa npX (Z :. (2::Int) :. (2::Int))
  print $ toList r'


  let x = fromList (ix2 2 2) [8::Double,1,3,5]
  z <- predict et x
  print $ toList z
  return ()
  
