module Main where

import Data.Aeson hiding ((.:))
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.Async
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa
import qualified Data.Vector.Storable as V
import SKLearn.PyInterOp
import SKLearn.Classes
import SKLearn.ExtraTrees


main :: IO ()
main = do
  mvarIn <- newEmptyMVar :: IO (MVar PyCallRequest)
  mvarOut <- newEmptyMVar :: IO (MVar Value)
  -- putMVar mvarIn $ PyCallRequest "test" [] HM.empty
  async $ runInterpreter mvarIn mvarOut  
  -- res <- takeMVar mvarOut
  -- print res

  threadDelay 1000000
  et <- new defaultExtraTreesRegressorParams :: IO (ExtraTreesRegressor)
  let x = fromForeignPtr (Z :. 2 :. 2) $ fst . V.unsafeToForeignPtr0 $ V.fromList [1,2,3,4]
      y = fromForeignPtr (Z :. 2) $ fst . V.unsafeToForeignPtr0 $ V.fromList [5,3]
  fitS et x y
  return ()
  
