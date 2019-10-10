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
import qualified Debug.Trace as Debug
import qualified Data.HashMap.Strict as HM
import System.Environment
import System.IO.Unsafe
import Control.Concurrent.Async
import Control.Concurrent.MVar
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

data PyInterpreter = PyInterpreter {stopInterpreter :: IO ()
                                   ,interpreterThread :: Async ()}

class ToPyArgument a where
  toPyArgument :: (MonadIO m, MonadMask m) => a -> m PyObject

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

withGIL :: (MonadIO m, MonadMask m) => m a -> m a
withGIL = bracket (liftIO pyGILStateEnsure) (liftIO . pyGILStateRelease) . const


str :: String -> (MonadIO m, MonadMask m) => m PyObject
str s = liftIO $ pyUnicodeDecodeLocale s "strict" >>= excIfNull

readStr :: (MonadIO m, MonadMask m) => PyObject -> m String
readStr = liftIO . pyUnicodeAsUTF8


importModule :: (MonadIO m, MonadMask m) => String -> m PyObject
importModule s = liftIO $ pyImportImportModule s >>= excIfNull


getAttr :: (MonadIO m, MonadMask m) => PyObject -> String -> m PyObject
getAttr pobj attr = liftIO $ pyObjectGetAttrString pobj attr >>= excIfNull


-- Return a python object representing an instance of our main interop class
initialize :: (MonadIO m, MonadMask m) => m ()
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


createArgsTuple :: (MonadIO m, MonadMask m) => [SomePyArgument] -> m PyObject
createArgsTuple args = do
  pArgs <- liftIO $ pyTupleNew (fromIntegral $ length args) >>= excIfNull
  for_ (zip args [0..]) $ \(SomePyArgument a, i) -> do
    arg <- toPyArgument a
    liftIO $ pyTupleSetItem pArgs i arg >>= excIfMinus1
  return pArgs


oldCallMethod :: (MonadIO m, MonadMask m) => PyObject -> PyCallRequest -> m PyObject
oldCallMethod obj PyCallRequest{methodName, args, kwargs} = do
  pArgs <- createArgsTuple args
  method <- getAttr obj methodName
  res <- liftIO $ pyObjectCallObject method pArgs >>= excIfNull
  liftIO $ pyDecRef pArgs
  return res


callMethodJSON ::(MonadIO m, MonadMask m, FromJSON a)
               => PyObject 
               -> String 
               -> [SomePyArgument] 
               -> m (Maybe a)
callMethodJSON obj methodName args = do
  res <- simpleCallMethod obj methodName args
  jsonRes <- jsonify res
  case eitherDecodeStrict' (BSC.pack jsonRes) of
    Right json -> return json
    Left e -> throwM $ userError e

simpleCallMethod :: (MonadIO m, MonadMask m) 
                 => PyObject 
                 -> String 
                 -> [SomePyArgument] 
                 -> m PyObject
simpleCallMethod obj methodName args = do
  debug env $ "Creating arguments for "++methodName
  pArgs <- createArgsTuple args
  debug env $ "Getting method object "++methodName
  method <- getAttr obj methodName
  debug env $ "Calling "++methodName
  res <- liftIO $ pyObjectCallObject method pArgs >>= excIfNull
  debug env $ "Call returned "++methodName
  liftIO $ pyDecRef pArgs
  debug env "Exiting from simpleCallMethod"
  return res

jsonify :: (MonadIO m, MonadMask m) => PyObject -> m String
jsonify obj = do
  pModule <- importModule "json"
  dumps <- getAttr pModule "dumps"
  pArgs <- createArgsTuple [SomePyArgument obj]
  strRes <- liftIO $ pyObjectCallObject dumps pArgs >>= excIfNull
  liftIO $ pyDecRef pArgs
  readStr strRes


-- | Starts the Python interpreter and hands over control
runInterpreter :: IO PyInterpreter
runInterpreter = do
  ourpp <- getDataFileName "pybits"
  pythonPath0 <- lookupEnv "PYTHONPATH"
  let pythonPath = case pythonPath0 of
                     Nothing -> ourpp
                     Just s -> s ++ (searchPathSeparator:ourpp)
  debug env $ "Setting PYTHONPATH to \""++pythonPath++"\""
  printDebugInfo env
  stopMVar <- newEmptyMVar
  -- runResourceT $ do
  initialize
  debug env "Initializing numpy"
  liftIO $ initNumpy
  -- newNumpyDoubleArray [3,3]
  debug env "Initialized"
  thread <- async $ takeMVar stopMVar
  return $ PyInterpreter (putMVar stopMVar ()) thread


nullObject :: PyObject -> Bool
nullObject (PyObject fptr) = unsafePerformIO $
  withForeignPtr fptr $ \ptr -> return (nullPtr == ptr)


excIfNull :: (MonadMask m, MonadIO m) => PyObject -> m PyObject
excIfNull po = do
  if nullObject po
     then do
       debug env "Null ptr!!"
       -- liftIO $ pyErrPrintEx 0
       raisePythonException
     else return po


whenM :: Monad m => m Bool -> m () -> m ()
whenM condM a = do
  cond <- condM
  if cond
     then a
     else return ()


raisePythonException :: forall m a. (MonadMask m, MonadIO m) => m a
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
    joinPyStringList :: PyObject -> m String
    joinPyStringList po = do
      nl <- str "\\n"
      s <- simpleCallMethod nl "join" [SomePyArgument po]
      readStr s


excIfMinus1 :: (MonadIO m, Num i, Eq i) => i -> m i
excIfMinus1 i
  | i == (-1) = liftIO $ pyErrPrintEx 0 >> return i
  | otherwise = return i


newNumpyDoubleArray :: forall m. (MonadIO m, MonadMask m) => [Int] -> m PyObject
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


repaToNumpy :: forall m sh. (MonadMask m, MonadIO m, Shape sh)
            => Array F sh Double -> m PyObject
repaToNumpy arr = do
  let dims = listOfShape (extent arr)
  dimsP <- liftIO $ newArray $ fromIntegral <$> dims :: m (Ptr CLong)
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

disableWarnings :: (MonadMask m, MonadIO m) => m ()
disableWarnings = do
  interop <- importModule "interop"
  simpleCallMethod interop "disable_warnings" []
  return ()
  return ()


runSimpleString :: String -> IO Int
runSimpleString s = fmap fromIntegral $ withCString s $ \cs -> 
  [C.exp| int {PyRun_SimpleString($(char* cs))} |]
