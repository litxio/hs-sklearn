{-# LANGUAGE ForeignFunctionInterface, ExtendedDefaultRules, GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
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
import qualified Data.ByteString.Lazy.Char8 as BSLC
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
import qualified Data.Aeson as A
import Foreign.Marshal.Array
import Control.Monad
import qualified Control.Exception (Exception)
import Data.Foldable
-- import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Paths_sklearn
import qualified Language.C.Inline as C
import Control.Monad.State

import SKLearn.PyInterOp.Python
import SKLearn.PyInterOp.Numpy

newtype Py a = Py { unPy :: StateT Bool IO a }
  deriving (Functor, Applicative, Monad, MonadState Bool, MonadIO
           ,MonadCatch, MonadThrow, MonadMask)

data PythonException = PythonException {description :: String
                                       ,traceback :: String}
                       | Unsupported {description :: String}
  deriving Show
instance Exception PythonException

type Env = ()
env = ()


debug :: MonadIO m => () -> String -> m ()
debug env s = liftIO $ return ()
-- debug env s = liftIO $ Debug.traceIO s

data PythonCommand = forall a. PythonCommand 
                                {action :: Py a
                                ,outbox :: MVar (Either PythonException a)}

data PyInterpreter = PyInterpreter {stopInterpreter :: IO ()
                                   ,commandMVar :: MVar PythonCommand
                                   ,interpreterThread :: Async ()}

class ToPyArgument a where
  toPyArgument ::  a -> Py PyObject

instance ToPyArgument PyObject where
  toPyArgument = return

instance ToPyArgument String where
  toPyArgument = str

instance ToPyArgument Int64 where
  toPyArgument = liftIO . pyLongFromLong

instance ToPyArgument Int where
  toPyArgument = liftIO . pyLongFromLong . fromIntegral

instance Shape sh => ToPyArgument (Array F sh Double) where
  toPyArgument = repaToNumpy

data SomePyArgument = forall a. ToPyArgument a => SomePyArgument a

withGIL ::  IO a -> IO a
withGIL = runInBoundThread . bracket pyGILStateEnsure pyGILStateRelease . const


str :: String -> Py PyObject
str s = liftIO (pyUnicodeDecodeLocale s "strict") >>= excIfNull

readStr ::  PyObject -> Py String
readStr = liftIO . pyUnicodeAsUTF8


importModule ::  String -> Py PyObject
importModule s = liftIO (pyImportImportModule s) >>= excIfNull


getAttr ::  PyObject -> String -> Py PyObject
getAttr pobj attr = liftIO (pyObjectGetAttrString pobj attr) >>= excIfNull


-- Return a python object representing an instance of our main interop class
initialize ::  Py ()
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


touchPyObject :: PyObject -> Py ()
touchPyObject (PyObject fp) = liftIO $ touchForeignPtr fp


createArgsTuple ::  [SomePyArgument] -> Py PyObject
createArgsTuple args = do
  pArgs <- liftIO (pyTupleNew (fromIntegral $ length args)) >>= excIfNull
  for_ (zip args [0..]) $ \(SomePyArgument a, i) -> do
    arg <- toPyArgument a
    -- Note that pyTupleSetItem steals our reference, which we don't want.  We
    -- need to call Py_IncRef to counteract that, because the ForeignPtr 
    -- finalizer is going to call Py_DecRef for us
    liftIO $ pyIncRef arg
    liftIO (pyTupleSetItem pArgs i arg) >>= excIfMinus1
  return pArgs


callMethodJSON :: A.FromJSON a
               => PyObject 
               -> String 
               -> [SomePyArgument] 
               -> Py (Maybe a)
callMethodJSON obj methodName args = do
  res <- simpleCallMethod obj methodName args
  jsonRes <- jsonify res
  case A.eitherDecodeStrict' (BSC.pack jsonRes) of
    Right json -> return json
    Left e -> throwM $ userError e

simpleCallMethod :: PyObject 
                 -> String 
                 -> [SomePyArgument] 
                 -> Py PyObject
simpleCallMethod obj methodName args = do
  debug env $ "Creating arguments for "++methodName
  pArgs <- createArgsTuple args
  debug env $ "Getting method object "++methodName
  method <- getAttr obj methodName
  debug env $ "Calling "++methodName
  res <- liftIO (pyObjectCallObject method pArgs) >>= excIfNull
  touchPyObject pArgs
  debug env $ "Call returned "++methodName
  debug env "Exiting from simpleCallMethod"
  return res

jsonify ::  PyObject -> Py String
jsonify obj = do
  pModule <- importModule "json"
  dumps <- getAttr pModule "dumps"
  pArgs <- createArgsTuple [SomePyArgument obj]
  strRes <- liftIO (pyObjectCallObject dumps pArgs) >>= excIfNull
  touchPyObject pArgs
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
  setEnv "PYTHONPATH" pythonPath
  printDebugInfo env
  stopMVar <- newEmptyMVar
  commandMVar <- newEmptyMVar
  debug env "Initializing numpy"
  
  debug env "Initialized"
  thread <- asyncBound $ forever $ do
    PythonCommand{action=Py action, outbox} <- takeMVar commandMVar
    catch (try (evalStateT action False) >>= putMVar outbox)
          (\(e::SomeException) ->
            let msg = "Unexpected exception in Python thread: "++show e
             in traceIO msg >> error msg)

  let interp = PyInterpreter (putMVar stopMVar ()) commandMVar thread
  runPython interp (initialize >> liftIO initNumpy)
  async $ watchForDecrefs commandMVar
  return interp
  where
    watchForDecrefs commandMVar = do
      outbox <- newEmptyMVar
      decrefQueue <- getDecrefQueue
      forever $ do
        debug env "Going to block and pop a a decref request"
        pyObjPtr :: Ptr PyObject <- castPtr <$> gAsyncQueueTimeoutPop decrefQueue 1000000
        if pyObjPtr == nullPtr
           then do
             queueLen <- gAsyncQueueLength decrefQueue
             debug env $ "Timed out; queue length is "++show queueLen
           else do
             debug env "Got decref on queue, about to call Py_DecRef"
             putMVar commandMVar $ PythonCommand (liftIO $ pyDecRefRawPtr pyObjPtr) outbox
             takeMVar outbox >> return ()


runPython :: PyInterpreter -> Py a -> IO a
runPython PyInterpreter{ commandMVar } action = do
  outbox <- newEmptyMVar
  let command = PythonCommand{action, outbox}
  putMVar commandMVar command
  takeMVar outbox >>= \case
    Left e -> debug env (show e) >> throwM e
    Right a -> return a


nullObject :: PyObject -> Bool
nullObject (PyObject fptr) = unsafePerformIO $
  withForeignPtr fptr $ \ptr -> return (nullPtr == ptr)


excIfNull :: PyObject -> Py PyObject
excIfNull po = do
  if nullObject po
     then do
       -- debug env "Null ptr!!"
       -- liftIO $ pyErrPrintEx 0
       raisePythonException
     else return po


whenM :: IO Bool -> IO () -> IO ()
whenM condM a = do
  cond <- condM
  if cond
     then a
     else return ()


repr :: PyObject -> Py String
repr obj = do
  mBuiltin <- importModule "builtins"
  simpleCallMethod mBuiltin "repr" [SomePyArgument obj] >>= readStr

pythonStrValue :: PyObject -> Py String
pythonStrValue obj = do
  mBuiltin <- importModule "builtins"
  simpleCallMethod mBuiltin "str" [SomePyArgument obj] >>= readStr


raisePythonException :: forall a. Py a
raisePythonException = do
  alreadyRaising <- get
  put True
  when alreadyRaising $ do
     liftIO $ handleAll (const $ return ()) $ pyErrPrintEx 0
     resetAndThrow $ PythonException ("A python exception occurred, and then ANOTHER exception"
                                     <> " occurred while trying to get the details.  More"
                                     <> " information may be above")
                                     ""

  (ptype, pvalue, ptraceback) <- liftIO pyErrFetch
  when (nullObject ptype || nullObject pvalue) $
    error "raisePythonException called with no Python exception!"
  
  tbModule <- importModule "traceback"
  exc <- handleAll (const $ liftIO $ PyObject <$> newForeignPtr_ nullPtr) $
                simpleCallMethod tbModule "format_exception_only"
                          [SomePyArgument ptype, SomePyArgument pvalue]
  when (nullObject exc) $ do
    typeRepr <- handleAll (const $ return "<couldn't get type>") (pythonStrValue ptype)
    valueRepr <- handleAll (const $ return "<couldn't get value>") (pythonStrValue pvalue)
    tbRepr <- handleAll (const $ return "<couldn't get traceback>") (pythonStrValue ptraceback)
    resetAndThrow $ PythonException (typeRepr<>": "<>valueRepr) tbRepr

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
  put False
  resetAndThrow $ PythonException excS tbS

  where
    joinPyStringList :: PyObject -> Py String
    joinPyStringList po = do
      nl <- str "\\n"
      s <- simpleCallMethod nl "join" [SomePyArgument po]
      readStr s

    resetAndThrow e = put False >> throwM e


excIfMinus1 :: (Eq i, Num i) => i -> Py i
excIfMinus1 i
  | i == (-1) = liftIO $ pyErrPrintEx 0 >> return i
  | otherwise = return i


newNumpyDoubleArray :: forall m.  [Int] -> IO PyObject
newNumpyDoubleArray dims = liftIO $ do
  dimsP <- newArray $ fromIntegral <$> dims :: IO (Ptr CLong)
  npArraySimpleNew (fromIntegral $ length dims) dimsP npDoubleType


fromPyDouble :: PyObject -> Py Double
fromPyDouble obj = do
  dbl <- liftIO $ pyFloatAsDouble obj
  err <- liftIO$  pyErrOccurred
  when (not $ nullObject err) raisePythonException
  return dbl


pyNew :: String -> String -> A.Value -> Py PyObject
pyNew moduleName className params = do
  debug env $ "pyNew getting module "++ moduleName
  mod <- importModule moduleName
  debug env $ "pyNew getting class "++ className
  pClass <- getAttr mod className

  debug env $ "pyNew creating args tuple class "++ className
  pArgs <- jsonToPyArgs params
  debug env $ "pyNew calling "++ className
  res <- case params of
    A.Array _ -> liftIO (pyObjectCallObject pClass pArgs) >>= excIfNull
    A.Object _ -> do
      emptyTuple <- liftIO (pyTupleNew 0) >>= excIfNull
      liftIO (pyObjectCall pClass emptyTuple pArgs) >>= excIfNull
  return res


jsonToPyArgs ::  A.Value -> Py PyObject
jsonToPyArgs obj = do
  mJson <- importModule "json"
  mBuiltin <- importModule "builtins"
  case obj of
     A.Array _ -> do
       pyArgL <- simpleCallMethod mJson "loads"
                    [SomePyArgument $ BSLC.unpack $ A.encode obj]
       simpleCallMethod mBuiltin "tuple" [SomePyArgument pyArgL]
     A.Object _ -> do
       simpleCallMethod mJson "loads"
                    [SomePyArgument $ BSLC.unpack $ A.encode obj]
     other -> throwM $ Unsupported $ "Only JSON arrays and objects are currently"
                                     ++" supported as method arguments"


repaToNumpy :: forall sh. Shape sh
            => Array F sh Double -> Py PyObject
repaToNumpy arr = do
  let dims = listOfShape (extent arr)
  dimsP <- liftIO $ newArray $ fromIntegral <$> dims :: Py (Ptr CLong)
  debug env "About to create array... hold on!!"
  liftIO (withForeignPtr (toForeignPtr arr) $ \p ->
    npArraySimpleNewFromData (fromIntegral $ length dims) dimsP npDoubleType (castPtr p))
        >>= excIfNull


numpyToRepa :: Shape sh => PyObject -> sh -> Py (Array F sh Double)
numpyToRepa npArr shape = do
  fPtr <- liftIO $ npArrayData npArr
  return $ fromForeignPtr shape fPtr


printDebugInfo :: Env -> IO ()
printDebugInfo env = do
  debug env $ "Python thread starting"
  pyVer <- pyGetVersion
  pyPath <- pyGetPath
  debug env $ "Python version " ++ pyVer
  debug env $ "Python module path: " ++ pyPath


disableWarnings :: Py ()
disableWarnings = do
  interop <- importModule "interop"
  simpleCallMethod interop "disable_warnings" []
  return ()


runSimpleString :: String -> Py Int
runSimpleString s = liftIO $ fmap fromIntegral $ withCString s $ \cs -> 
  [C.exp| int {PyRun_SimpleString($(char* cs))} |]
