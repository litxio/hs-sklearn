module Main where

import SKLearn.PyInterOp
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.MVar
import Control.Concurrent.Async

main :: IO ()
main = do
  mvarIn <- newEmptyMVar :: IO (MVar PyCallRequest)
  mvarOut <- newEmptyMVar :: IO (MVar Value)
  putMVar mvarIn $ PyCallRequest "test" [] HM.empty
  async $ runInterpreter mvarIn mvarOut  
  res <- takeMVar mvarOut
  print res
