{-# LANGUAGE ForeignFunctionInterface, ExtendedDefaultRules, GADTs #-}

module SKLearn.PyInterOp where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString.Char8 as BSC
import qualified Debug.Trace as Debug
import qualified Data.HashMap.Strict as HM
import System.Environment
import Control.Concurrent.Async
import Control.Concurrent.MVar
import System.FilePath
import Data.String
import Data.Aeson
import Foreign.Marshal.Array
import Control.Monad
import Data.Foldable
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Paths_sklearn

data PyObject = PyObject
type PyObjectPtr = Ptr PyObject

type Env = ()
env = ()

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

data SomePyArgument = forall a. ToPyArgument a => SomePyArgument a

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


callMethod :: PyObjectPtr -> PyCallRequest -> ResIO PyObjectPtr
callMethod obj PyCallRequest{methodName, args, kwargs} = do
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
runInterpreter :: MVar PyCallRequest -> MVar Value -> IO ()
runInterpreter mvarIn mvarOut = do
  ourpp <- getDataFileName "pybits"
  pythonPath0 <- lookupEnv "PYTHONPATH"
  let pythonPath = case pythonPath0 of
                     Nothing -> ourpp
                     Just s -> s ++ (searchPathSeparator:ourpp)
  debug env $ "Setting PYTHONPATH to \""++pythonPath++"\""
  printDebugInfo env
  runResourceT $ do
    interopObj <- initialize
    debug env "Initialized"
    let loop = do
          req <- liftIO $ takeMVar mvarIn
          debug env "Got request"
          res <- callMethod interopObj req
          jsonRes <- jsonify res
          case eitherDecodeStrict' (BSC.pack jsonRes) of
            Right json -> liftIO $ putMVar mvarOut json
            Left e -> throwM $ userError e
          loop
    loop

excIfNull :: MonadIO m => PyObjectPtr -> m PyObjectPtr
excIfNull p
  | p == nullPtr = liftIO $ pyErrPrintEx 0 >> return p
  | otherwise = return p

excIfMinus1 :: (MonadIO m, Num i, Eq i) => i -> m i
excIfMinus1 i
  | i == (-1) = liftIO $ pyErrPrintEx 0 >> return i
  | otherwise = return i


printDebugInfo :: Env -> IO ()
printDebugInfo env = do
  debug env $ "Python thread starting"
  pyVer <- pyGetVersion >>= peekCString
  pyPath <- pyGetPath >>= peekCWString
  debug env $ "Python version " ++ pyVer
  debug env $ "Python module path: " ++ pyPath

debug env = liftIO . Debug.traceIO

foreign import ccall "Py_Main" pyMain :: CInt -> Ptr (Ptr CInt) -> IO CInt
foreign import ccall "PyRun_SimpleString" pyRunSimpleString :: CString -> IO CInt
foreign import ccall "Py_Initialize" pyInitialize :: IO ()
foreign import ccall "Py_Finalize" pyFinalize :: IO ()
foreign import ccall "Py_AddPendingCall" pyAddPendingCall :: FunPtr (IO CInt) -> IO CInt

-- The result of pyGILStateEnsure is actually an enum not int so I am violating
-- an abstraction here.
foreign import ccall "PyGILState_Ensure" pyGILStateEnsure :: IO CInt
foreign import ccall "PyGILState_Release" pyGILStateRelease :: CInt -> IO ()
foreign import ccall "&PyExc_EOFError" pyEOFError :: Ptr PyObjectPtr

foreign import ccall "Py_GetVersion" pyGetVersion :: IO CString
foreign import ccall "Py_GetPath" pyGetPath :: IO CWString
foreign import ccall "Py_DecRef" pyDecRef :: PyObjectPtr -> IO ()

foreign import ccall "wrapper" createPendingCallPtr :: IO CInt -> IO (FunPtr (IO CInt))

foreign import ccall "PyErr_SetInterrupt" pyErrSetInterrupt :: IO ()
foreign import ccall "PyErr_CheckSignals" pyErrCheckSignals :: IO CInt
foreign import ccall "PyErr_SetString" pyErrSetString :: PyObjectPtr -> CString -> IO ()
foreign import ccall "PyErr_PrintEx" pyErrPrintEx :: Int -> IO ()

foreign import ccall "PyUnicode_DecodeFSDefault" 
  pyUnicodeDecodeFSDefault :: CString -> IO PyObjectPtr
foreign import ccall "PyUnicode_AsUTF8" 
  pyUnicodeAsUTF8 :: PyObjectPtr -> IO CString


foreign import ccall "PyImport_ImportModule" 
  pyImportImportModule :: CString -> IO PyObjectPtr

foreign import ccall "PyTuple_New" 
  pyTupleNew :: CInt -> IO PyObjectPtr
foreign import ccall "PyTuple_SetItem" 
  pyTupleSetItem :: PyObjectPtr -> CInt -> PyObjectPtr -> IO CInt

foreign import ccall "PyObject_CallObject" 
  pyObjectCallObject :: PyObjectPtr -> PyObjectPtr -> IO PyObjectPtr
foreign import ccall "PyObject_GetAttrString" 
  pyObjectGetAttrString :: PyObjectPtr -> CString -> IO PyObjectPtr


foreign import ccall "PyLong_FromLong" 
  pyLongFromLong :: CLong -> IO PyObjectPtr


