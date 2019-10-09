{-# LANGUAGE ForeignFunctionInterface, ExtendedDefaultRules, GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SKLearn.PyInterOp
  (module SKLearn.PyInterOp ,PyObject, PyObjectPtr
  )where

import Foreign
import qualified Foreign.Concurrent as FC
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Repr.ForeignPtr (F, toForeignPtr, fromForeignPtr)
import qualified Data.ByteString.Char8 as BSC
import qualified Debug.Trace as Debug
import qualified Data.HashMap.Strict as HM
import System.Environment
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.Catch
import qualified Data.Vector.Storable as V
import System.FilePath
import Data.String
import Data.Aeson hiding (Array)
import Foreign.Marshal.Array
import Control.Monad
import Data.Foldable
-- import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Paths_sklearn
import qualified Language.C.Inline as C

import SKLearn.PyInterOp.CImports

allocate a _ = (0,) <$> a

type Env = ()
env = ()
type ResIO = IO

class ToPyArgument a where
  toPyArgument :: a -> ResIO PyObjectPtr

instance ToPyArgument PyObjectPtr where
  toPyArgument = return

instance ToPyArgument String where
  toPyArgument = str

instance ToPyArgument Int64 where
  toPyArgument = mPtr . pyLongFromLong . CLong

instance ToPyArgument Int where
  toPyArgument = mPtr . pyLongFromLong . CLong . fromIntegral

instance Shape sh => ToPyArgument (Array F sh Double) where
  toPyArgument = repaToNumpy

data SomePyArgument = forall a. ToPyArgument a => SomePyArgument a

withGIL :: ResIO a -> ResIO a
withGIL = bracket pyGILStateEnsure pyGILStateRelease . const


mPtr :: IO PyObjectPtr -> ResIO PyObjectPtr
mPtr po = snd <$> allocate po pyDecRef

str :: String -> ResIO PyObjectPtr
str s = do
  cs <- snd <$> allocate (newCString s) free
  mPtr $ pyUnicodeDecodeFSDefault cs

readStr :: PyObjectPtr -> ResIO String
readStr = liftIO . (pyUnicodeAsUTF8 >=> peekCString)


importModule :: String -> ResIO PyObjectPtr
importModule s = liftIO $ withCString s $ \cs ->
  pyImportImportModule cs


getAttr :: PyObjectPtr -> String -> ResIO PyObjectPtr
getAttr pobj attr = liftIO $ withCString attr $ \cattr ->
  pyObjectGetAttrString pobj cattr >>= excIfNull


-- Return a python object representing an instance of our main interop class
initialize :: ResIO PyObjectPtr
initialize = do
  liftIO $ pyInitialize
  debug env "Importing interop"
  pModule <- importModule "interop" >>= excIfNull
  pClass <- getAttr pModule "Interop" >>= excIfNull
  mPtr $ pyObjectCallObject pClass nullPtr >>= excIfNull
  --when (res == -1) $
  --  logerr env $ "Python exited with an exception"
  -- pyFinalize
  -- putMVar done ()


data PyCallRequest = 
  PyCallRequest { methodName :: String
                , args :: [SomePyArgument]
                , kwargs :: HM.HashMap String [SomePyArgument] }


createArgsTuple :: [SomePyArgument] -> ResIO PyObjectPtr
createArgsTuple args = do
  pArgs <- mPtr $ pyTupleNew (fromIntegral $ length args) >>= excIfNull
  for_ (zip args [0..]) $ \(SomePyArgument a, i) -> do
    arg <- toPyArgument a
    liftIO $ pyTupleSetItem pArgs i arg >>= excIfMinus1
  return pArgs


oldCallMethod :: PyObjectPtr -> PyCallRequest -> ResIO PyObjectPtr
oldCallMethod obj PyCallRequest{methodName, args, kwargs} = do
  pArgs <- createArgsTuple args
  method <- getAttr obj methodName
  res <- mPtr $ pyObjectCallObject method pArgs >>= excIfNull
  liftIO $ pyDecRef pArgs
  return res


simpleCallMethod :: PyObjectPtr 
                 -> String 
                 -> [SomePyArgument] 
                 -> ResIO PyObjectPtr
simpleCallMethod obj methodName args = do
  pArgs <- createArgsTuple args
  method <- getAttr obj methodName
  res <- mPtr $ pyObjectCallObject method pArgs >>= excIfNull
  liftIO $ pyDecRef pArgs
  return res

jsonify :: PyObjectPtr -> ResIO String
jsonify obj = do
  pModule <- importModule "json"
  dumps <- getAttr pModule "dumps"
  pArgs <- createArgsTuple [SomePyArgument obj]
  strRes <- mPtr $ pyObjectCallObject dumps pArgs >>= excIfNull
  liftIO $ pyDecRef pArgs
  readStr strRes


-- | Starts the Python interpreter and hands over control
runInterpreter :: MVar PyCallRequest -> MVar Value -> IO (Async ())
runInterpreter mvarIn mvarOut = do
  ourpp <- getDataFileName "pybits"
  pythonPath0 <- lookupEnv "PYTHONPATH"
  let pythonPath = case pythonPath0 of
                     Nothing -> ourpp
                     Just s -> s ++ (searchPathSeparator:ourpp)
  debug env $ "Setting PYTHONPATH to \""++pythonPath++"\""
  printDebugInfo env
  -- runResourceT $ do
  do
    interopObj <- initialize
    liftIO $ initNumpy
    -- newNumpyDoubleArray [3,3]
    debug env "Initialized"
    let loop = do
          req <- liftIO $ takeMVar mvarIn
          debug env "Got request"
          res <- oldCallMethod interopObj req
          jsonRes <- jsonify res
          case eitherDecodeStrict' (BSC.pack jsonRes) of
            Right json -> liftIO $ putMVar mvarOut json
            Left e -> throwM $ userError e
          loop
    async loop

excIfNull :: MonadIO m => PyObjectPtr -> m PyObjectPtr
excIfNull p
  | p == nullPtr = liftIO $ pyErrPrintEx 0 >> return p
  | otherwise = return p

excIfMinus1 :: (MonadIO m, Num i, Eq i) => i -> m i
excIfMinus1 i
  | i == (-1) = liftIO $ pyErrPrintEx 0 >> return i
  | otherwise = return i


newNumpyDoubleArray :: [Int] -> ResIO PyObjectPtr
newNumpyDoubleArray dims = do
  dimsP <- snd <$> allocate (newArray $ fromIntegral <$> dims) free :: ResIO (Ptr CLong)
  liftIO $ castPtr <$> npArraySimpleNew 
                          (fromIntegral $ length dims) dimsP npDoubleType


pyNew :: String -> String -> Value -> IO PyObjectPtr
pyNew moduleName className params = do
  mod <- importModule moduleName
  pClass <- getAttr mod className
  -- TODO handle args
  pArgs <- createArgsTuple []
  mPtr $ pyObjectCallObject pClass pArgs >>= excIfNull


repaToNumpy :: Shape sh => Array F sh Double -> IO PyObjectPtr
repaToNumpy arr = do
  let dims = listOfShape (extent arr)
  dimsP <- snd <$> allocate (newArray $ fromIntegral <$> dims) free :: IO (Ptr CLong)
  debug env "About to create array... hold on!!"
  withForeignPtr (toForeignPtr arr) $ \p ->
    npArraySimpleNewFromData (fromIntegral $ length dims) dimsP npDoubleType (castPtr p)
        >>= excIfNull


numpyToRepa :: Shape sh => PyObjectPtr -> sh -> IO (Array F sh Double)
numpyToRepa npArr shape = do
  dataPtr <- castPtr <$> npArrayData npArr
  -- let cleanup = castFunPtr $ [C.funPtr| void deref(double* ptr) {
  --                                         Py_DecRef( $(void* npArr)); 
  --                                       } |]
  fPtr <- FC.newForeignPtr dataPtr (pyDecRef npArr>>return ())
  return $ fromForeignPtr shape fPtr


printDebugInfo :: Env -> IO ()
printDebugInfo env = do
  debug env $ "Python thread starting"
  pyVer <- pyGetVersion >>= peekCString
  pyPath <- pyGetPath >>= peekCWString
  debug env $ "Python version " ++ pyVer
  debug env $ "Python module path: " ++ pyPath

debug env = liftIO . Debug.traceIO

