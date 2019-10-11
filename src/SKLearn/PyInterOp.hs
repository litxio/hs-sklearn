{-# LANGUAGE ForeignFunctionInterface, ExtendedDefaultRules, GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module SKLearn.PyInterOp
  (module SKLearn.PyInterOp ,PyObject
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
import Debug.Trace as Debug
import qualified Data.HashMap.Strict as HM
import System.Environment
import System.IO.Unsafe
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.Catch
import qualified Data.Vector.Storable as V
import System.FilePath
import Data.String
import Data.Aeson hiding (Array)
import Foreign.Marshal.Array
import Control.Monad
import qualified Control.Exception (Exception)
import Data.Foldable
-- import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Paths_sklearn
import qualified Language.C.Inline as C

import SKLearn.PyInterOp.Python
import SKLearn.PyInterOp.Numpy

data PythonException = PythonException {description :: String
                                       ,traceback :: String}
  deriving (Show, Exception)

type Env = ()
env = ()

data PythonCommand = forall a. PythonCommand 
                                {action :: IO a
                                ,outbox :: MVar (Either PythonException a)}

data PyInterpreter = PyInterpreter {stopInterpreter :: IO ()
                                   ,commandMVar :: MVar PythonCommand
                                   ,interpreterThread :: Async ()}

class ToPyArgument a where
  toPyArgument ::  a -> IO PyObject

instance ToPyArgument PyObject where
  toPyArgument = return

instance ToPyArgument String where
  toPyArgument = str

instance ToPyArgument Int64 where
  toPyArgument = liftIO . pyLongFromLong

instance ToPyArgument Int where
  toPyArgument = liftIO . pyLongFromLong . fromIntegral

instance Shape sh => ToPyArgument (Array F sh Double) where
  toPyArgument = liftIO . repaToNumpy

data SomePyArgument = forall a. ToPyArgument a => SomePyArgument a

withGIL ::  IO a -> IO a
withGIL = runInBoundThread . bracket pyGILStateEnsure pyGILStateRelease . const


str :: String ->  IO PyObject
str s = liftIO $ pyUnicodeDecodeLocale s "strict" >>= excIfNull

readStr ::  PyObject -> IO String
readStr = liftIO . pyUnicodeAsUTF8


importModule ::  String -> IO PyObject
importModule s = liftIO $ pyImportImportModule s >>= excIfNull


getAttr ::  PyObject -> String -> IO PyObject
getAttr pobj attr = liftIO $ pyObjectGetAttrString pobj attr >>= excIfNull


-- Return a python object representing an instance of our main interop class
initialize ::  IO ()
initialize = do
  liftIO $ pyInitialize
  disableWarnings
  -- debug env "Importing interop"
  -- pModule <- importModule "interop" >>= excIfNull
  -- pClass <- getAttr pModule "Interop" >>= excIfNull
  -- mPtr $ pyObjectCallObject pClass nullPtr >>= excIfNull
  --when (res == -1) $
  --  logerr env $ "Python exited with an exception"
  -- pyFinalize
  -- putMVar done ()


data PyCallRequest = 
  PyCallRequest { methodName :: String
                , args :: [SomePyArgument]
                , kwargs :: HM.HashMap String [SomePyArgument] }


createArgsTuple ::  [SomePyArgument] -> IO PyObject
createArgsTuple args = do
  pArgs <- liftIO $ pyTupleNew (fromIntegral $ length args) >>= excIfNull
  for_ (zip args [0..]) $ \(SomePyArgument a, i) -> do
    arg <- toPyArgument a
    -- Note that pyTupleSetItem steals our reference, which we don't want.  We
    -- need to call Py_IncRef to counteract that, because the ForeignPtr 
    -- finalizer is going to call Py_DecRef for us
    liftIO $ pyIncRef arg
    liftIO $ pyTupleSetItem pArgs i arg >>= excIfMinus1
  return pArgs


oldCallMethod ::  PyObject -> PyCallRequest -> IO PyObject
oldCallMethod obj PyCallRequest{methodName, args, kwargs} = do
  pArgs <- createArgsTuple args
  method <- getAttr obj methodName
  res <- liftIO $ pyObjectCallObject method pArgs >>= excIfNull
  return res


callMethodJSON :: FromJSON a
               => PyObject 
               -> String 
               -> [SomePyArgument] 
               -> IO (Maybe a)
callMethodJSON obj methodName args = do
  res <- simpleCallMethod obj methodName args
  jsonRes <- jsonify res
  case eitherDecodeStrict' (BSC.pack jsonRes) of
    Right json -> return json
    Left e -> throwM $ userError e

simpleCallMethod :: PyObject 
                 -> String 
                 -> [SomePyArgument] 
                 -> IO PyObject
simpleCallMethod obj methodName args = do
  debug env $ "Creating arguments for "++methodName
  pArgs <- createArgsTuple args
  debug env $ "Getting method object "++methodName
  method <- getAttr obj methodName
  debug env $ "Calling "++methodName
  res <- liftIO $ pyObjectCallObject method pArgs >>= excIfNull
  debug env $ "Call returned "++methodName
  debug env "Exiting from simpleCallMethod"
  return res

jsonify ::  PyObject -> IO String
jsonify obj = do
  pModule <- importModule "json"
  dumps <- getAttr pModule "dumps"
  pArgs <- createArgsTuple [SomePyArgument obj]
  strRes <- liftIO $ pyObjectCallObject dumps pArgs >>= excIfNull
  readStr strRes


-- | Starts the Python interpreter and hands over control
runInterpreter :: IO PyInterpreter
runInterpreter = runInBoundThread $ do
  ourpp <- getDataFileName "pybits"
  pythonPath0 <- lookupEnv "PYTHONPATH"
  let pythonPath = case pythonPath0 of
                     Nothing -> ourpp
                     Just s -> s ++ (searchPathSeparator:ourpp)
  debug env $ "Setting PYTHONPATH to \""++pythonPath++"\""
  printDebugInfo env
  stopMVar <- newEmptyMVar
  commandMVar <- newEmptyMVar
  -- runResourceT $ do
  debug env "Initializing numpy"
  
  -- newNumpyDoubleArray [3,3]
  debug env "Initialized"
  thread <- asyncBound $ forever $ do
    PythonCommand{action, outbox} <- takeMVar commandMVar
    try action >>= putMVar outbox

  let interp = PyInterpreter (putMVar stopMVar ()) commandMVar thread
  runPython interp (initialize >> initNumpy)
  return interp


runPython :: PyInterpreter -> IO a -> IO a
runPython PyInterpreter{ commandMVar } action = do
  outbox <- newEmptyMVar
  let command = PythonCommand{action, outbox}
  putMVar commandMVar command
  takeMVar outbox >>= \case
    Left e -> throwM e
    Right a -> return a


nullObject :: PyObject -> Bool
nullObject (PyObject fptr) = unsafePerformIO $
  withForeignPtr fptr $ \ptr -> return (nullPtr == ptr)


excIfNull :: PyObject -> IO PyObject
excIfNull po = do
  if nullObject po
     then do
       debug env "Null ptr!!"
       -- liftIO $ pyErrPrintEx 0
       raisePythonException
     else return po


whenM :: IO Bool -> IO () -> IO ()
whenM condM a = do
  cond <- condM
  if cond
     then a
     else return ()


raisePythonException :: forall a. IO a
raisePythonException = do
  (ptype, pvalue, ptraceback) <- liftIO pyErrFetch
  when (nullObject ptype || nullObject pvalue) $
    error "raisePythonException called with no Python exception!"
  
  tbModule <- importModule "traceback"
  exc <- simpleCallMethod tbModule "format_exception_only"
                          [SomePyArgument ptype, SomePyArgument pvalue]
  when (nullObject exc) $
     throwM $ userError $ "A python exception occurred, but there was an "
                          <> " error retrieving the exception details"
  excS <- joinPyStringList exc
  debug env $ "Exception was "<>excS

  tbS <- if nullObject ptraceback
            then return ""
            else do
              debug env "About to format_tb"
              tbStrList 
                <- simpleCallMethod tbModule "format_tb" [SomePyArgument ptraceback]
              liftIO $ pyErrPrintEx 0
              if (nullObject tbStrList)
                then return "There was an error retrieving the traceback"
                else joinPyStringList tbStrList
  throwM $ PythonException excS tbS

  where
    joinPyStringList :: PyObject -> IO String
    joinPyStringList po = do
      nl <- str "\\n"
      s <- simpleCallMethod nl "join" [SomePyArgument po]
      readStr s


excIfMinus1 :: (Eq i, Num i) => i -> IO i
excIfMinus1 i
  | i == (-1) = liftIO $ pyErrPrintEx 0 >> return i
  | otherwise = return i


newNumpyDoubleArray :: forall m.  [Int] -> IO PyObject
newNumpyDoubleArray dims = liftIO $ do
  dimsP <- newArray $ fromIntegral <$> dims :: IO (Ptr CLong)
  npArraySimpleNew (fromIntegral $ length dims) dimsP npDoubleType


pyNew :: String -> String -> Value -> IO PyObject
pyNew moduleName className params = do
  debug env $ "pyNew getting module "++ moduleName
  mod <- importModule moduleName
  debug env $ "pyNew getting class "++ className
  pClass <- getAttr mod className
  -- TODO handle args
  debug env $ "pyNew creating args tuple class "++ className
  pArgs <- createArgsTuple []
  debug env $ "pyNew calling "++ className
  pyObjectCallObject pClass pArgs >>= excIfNull


repaToNumpy :: forall sh. Shape sh
            => Array F sh Double -> IO PyObject
repaToNumpy arr = do
  let dims = listOfShape (extent arr)
  dimsP <- liftIO $ newArray $ fromIntegral <$> dims :: IO (Ptr CLong)
  debug env "About to create array... hold on!!"
  liftIO $ withForeignPtr (toForeignPtr arr) $ \p ->
    npArraySimpleNewFromData (fromIntegral $ length dims) dimsP npDoubleType (castPtr p)
        >>= excIfNull


numpyToRepa :: Shape sh => PyObject -> sh -> IO (Array F sh Double)
numpyToRepa npArr shape = do
  fPtr <- npArrayData npArr
  return $ fromForeignPtr shape fPtr




printDebugInfo :: Env -> IO ()
printDebugInfo env = do
  debug env $ "Python thread starting"
  pyVer <- pyGetVersion
  pyPath <- pyGetPath
  debug env $ "Python version " ++ pyVer
  debug env $ "Python module path: " ++ pyPath


debug env = liftIO . Debug.traceIO

disableWarnings :: IO ()
disableWarnings = do
  interop <- importModule "interop"
  simpleCallMethod interop "disable_warnings" []
  return ()
  return ()


runSimpleString :: String -> IO Int
runSimpleString s = fmap fromIntegral $ withCString s $ \cs -> 
  [C.exp| int {PyRun_SimpleString($(char* cs))} |]
