module Main where

import Data.Aeson hiding ((.:))
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.Async
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa
import Data.Array.Repa.Eval (fromList)
import Data.Array.Repa.Algorithms.Randomish
import Debug.Trace
import qualified Data.Vector.Storable as V
import SKLearn.PyInterOp
import SKLearn.Classes
import Control.Monad
import SKLearn.ExtraTrees
import SKLearn.LinearRegression


main :: IO ()
main = do
  itp <- runInterpreter
  -- res <- takeMVar mvarOut
  -- print res

  et <- new itp defaultExtraTreesRegressorParams :: IO (ExtraTreesRegressor)
  let x = fromList (ix2 2 2) [1::Double,2,3,4]
      y = fromList (ix1 2) [5::Double,3]
  fitS itp et x y

  traceIO "to numpy..."
  npX <- repaToNumpy x
  traceIO "to repa..."
  r' <- numpyToRepa npX (Z :. (2::Int) :. (2::Int))
  print $ toList r'


  let x = fromList (ix2 2 2) [8::Double,1,3,5]
  z <- predict itp et x
  print $ toList z
  
  replicateM_ 1000 $ do
    lr <- new itp defaultLinearRegressionParams
    xBig <- computeP $ delay $ randomishDoubleArray (ix2 1000 1000) (-100) 100 0
    yBig <- computeP $ delay $ randomishDoubleArray (ix1 1000) (-100) 100 0
    fitS itp lr xBig yBig
    zBig <- predict itp lr xBig
    return ()
    -- print $ toList zBig

  return ()
