module Main where

import Data.Aeson hiding ((.:), Array)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.Async
import Data.Array.Repa.Repr.ForeignPtr as Repa
import Data.Array.Repa as Repa
import Data.Array.Repa.Eval as Repa
import Data.Array.Repa.Algorithms.Randomish as Repa
import Data.Massiv.Array as Massiv hiding (new, (:.))
import Debug.Trace
import qualified Data.Vector.Storable as V
import System.Random as Random
import SKLearn.PyInterOp
import SKLearn.Classes
import Control.Monad
import SKLearn.ExtraTrees
import SKLearn.LinearRegression

main = do
  traceIO "Creating interpreter..."
  itp <- runInterpreter
  traceIO "Running with Massiv"
  massivMain itp
  traceIO "Running with Repa"
  repaMain itp

repaMain itp = do
  traceIO "Creating regressor..."
  et <- new itp defaultExtraTreesRegressorParams :: IO (ExtraTreesRegressor)
  let x = Repa.fromList (ix2 2 3) [1,2,3,4,5,6] :: Repa.Array F DIM2 Double
      y = Repa.fromList (ix1 2) [5,3] :: Repa.Array F DIM1 Double
  traceIO "Fitting regressor..."
  fitS itp et x y

  traceIO "to numpy..."
  npX <- runPython itp $ repaToNumpy x
  traceIO "to repa..."
  r' <- runPython itp $ numpyToRepa npX (Z :. (2::Int) :. (3::Int))
  print $ Repa.toList r'


  let x = Repa.fromList (ix2 2 3) [8::Double,1,3,5,7,4] :: Repa.Array F DIM2 Double 
  z <- predict itp et x :: IO (Repa.Array F DIM1 Double)
  print $ Repa.toList z
  
  replicateM_ 10 $ do
    lr <- new itp defaultLinearRegressionParams
    xBig <- computeP $ Repa.delay $ randomishDoubleArray (ix2 1000 1000) (-100) 100 0
      :: IO (Repa.Array F DIM2 Double)
    yBig <- computeP $ Repa.delay $ randomishDoubleArray (ix1 1000) (-100) 100 0
      :: IO (Repa.Array F DIM1 Double)
    fitS itp lr xBig yBig
    zBig <- predict itp lr xBig :: IO (Repa.Array F DIM1 Double)
    return ()
    -- print $ toList zBig

  return ()


massivMain itp = do
  traceIO "Creating regressor..."
  et <- new itp defaultExtraTreesRegressorParams :: IO (ExtraTreesRegressor)
  let x = Massiv.fromLists' Seq [[1,2,3],[4,5,6]] :: Massiv.Array S Ix2 Double
      y = Massiv.fromList Seq [5,3] :: Massiv.Array S Ix1 Double
  traceIO "Fitting regressor..."
  fitS itp et x y

  traceIO "to numpy..."
  npX <- runPython itp $ massivToNumpy x
  traceIO "to massiv..."
  r' <- runPython itp $ numpyToMassiv npX (Sz2 2 3)
  print $ Massiv.toList r'


  let x = Massiv.fromLists' Seq [[8,1,3],[5,7,4]] :: Massiv.Array S Ix2 Double 
  z <- predict itp et x :: IO (Massiv.Array S Ix1 Double)
  print $ Massiv.toList z
  
  replicateM_ 10 $ do
    lr <- new itp defaultLinearRegressionParams
    let xBig = compute $ randomArr (Sz2 1000 1000)
                :: Massiv.Array S Ix2 Double
        yBig = compute $ randomArr (Sz1 1000)
                :: Massiv.Array S Ix1 Double
    fitS itp lr xBig yBig
    zBig <- predict itp lr xBig :: IO (Massiv.Array S Ix1 Double)
    return ()
    -- print $ toList zBig

  return ()
  where
    randomArr :: Massiv.Index ix => Sz ix -> Massiv.Array S ix Double
    randomArr sz = 
      let gen = Random.mkStdGen 217
       in snd $ randomArrayS gen sz Random.random
