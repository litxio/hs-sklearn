{-# LANGUAGE ForeignFunctionInterface, ExtendedDefaultRules, GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SKLearn.PyInterOp.Numpy where

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
import Control.Concurrent
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

import SKLearn.PyInterOp.Python
import SKLearn.PyInterOp.Utils
import SKLearn.PyInterOp.TH

-- This gets rid of an annoying warning
C.verbatim "#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION"

C.include $ $(numpyIncludeDir) ++ "/numpy/arrayobject.h"

npDoubleType = [C.pure| int{NPY_DOUBLE} |]


-- foreign import ccall "Py_Main" pyMain :: CInt -> Ptr (Ptr CInt) -> IO CInt
-- foreign import ccall "PyRun_SimpleString" pyRunSimpleString :: CString -> IO CInt
-- foreign import ccall "Py_Initialize" pyInitialize :: IO ()
-- foreign import ccall "Py_Finalize" pyFinalize :: IO ()
-- foreign import ccall "Py_AddPendingCall" pyAddPendingCall :: FunPtr (IO CInt) -> IO CInt
-- 
-- -- The result of pyGILStateEnsure is actually an enum not int so I am violating
-- -- an abstraction here.
-- foreign import ccall "PyGILState_Ensure" pyGILStateEnsure :: IO CInt
-- foreign import ccall "PyGILState_Release" pyGILStateRelease :: CInt -> IO ()
--
-- foreign import ccall "&PyExc_EOFError" pyEOFError :: Ptr PyObjectPtr
-- 
-- foreign import ccall "Py_GetVersion" pyGetVersion :: IO CString
-- foreign import ccall "Py_GetPath" pyGetPath :: IO CWString
-- foreign import ccall "Py_DecRef" pyDecRef :: PyObjectPtr -> IO ()
-- foreign import ccall "Py_IncRef" pyIncRef :: PyObjectPtr -> IO ()
-- foreign import ccall "&Py_DecRef" pyDecRef_Ptr :: FunPtr (Ptr a -> IO ())
-- 
-- foreign import ccall "wrapper" createPendingCallPtr :: IO CInt -> IO (FunPtr (IO CInt))
-- 
-- foreign import ccall "PyErr_SetInterrupt" pyErrSetInterrupt :: IO ()
-- foreign import ccall "PyErr_CheckSignals" pyErrCheckSignals :: IO CInt
-- foreign import ccall "PyErr_SetString" pyErrSetString :: PyObjectPtr -> CString -> IO ()
-- foreign import ccall "PyErr_PrintEx" pyErrPrintEx :: Int -> IO ()
-- foreign import ccall "PyErr_Occurred" pyErrOccurred :: IO PyObjectPtr
-- foreign import ccall "PyErr_Fetch" pyErrFetch :: Ptr PyObjectPtr 
--                                               -> Ptr PyObjectPtr
--                                               -> Ptr PyObjectPtr
--                                               -> IO ()
-- 
-- foreign import ccall "PyUnicode_DecodeFSDefault" 
--   pyUnicodeDecodeFSDefault :: CString -> IO PyObjectPtr
-- foreign import ccall "PyUnicode_AsUTF8" 
--   pyUnicodeAsUTF8 :: PyObjectPtr -> IO CString
-- 
-- 
-- foreign import ccall "PyImport_ImportModule" 
--   pyImportImportModule :: CString -> IO PyObjectPtr
-- 
-- foreign import ccall "PyTuple_New" 
--   pyTupleNew :: CInt -> IO PyObjectPtr
-- foreign import ccall "PyTuple_SetItem" 
--   pyTupleSetItem :: PyObjectPtr -> CInt -> PyObjectPtr -> IO CInt
-- 
-- foreign import ccall "PyObject_CallObject" 
--   pyObjectCallObject :: PyObjectPtr -> PyObjectPtr -> IO PyObjectPtr
-- foreign import ccall "PyObject_GetAttrString" 
--   pyObjectGetAttrString :: PyObjectPtr -> CString -> IO PyObjectPtr
-- 
-- foreign import ccall "PyLong_FromLong" 
--   pyLongFromLong :: CLong -> IO PyObjectPtr

initNumpy :: IO ()
initNumpy = [C.block| void { import_array(); } |]


npArraySimpleNew :: CInt -> Ptr CLong -> CInt -> IO PyObject
npArraySimpleNew nd dims typenum = do
  arrP :: Ptr PyObject
    <- castPtr <$> [C.exp| void* {PyArray_SimpleNew($(int nd), $(long* dims), $(int typenum))} |]
  PyObject <$> newForeignPtr decref_with_gil arrP
     

-- | This gives us a weird sort of dependency where we want the original
-- PyObject to NOT get its reference count decreased until we are done with 
-- the resulting array.  
npArrayData :: PyObject -> IO (ForeignPtr a)
npArrayData arr@(PyObject fptr) =
  withForeignPtr fptr $ \ptr -> do
    let ptr' = castPtr ptr
    pyIncRef arr
    dataPtr <- castPtr <$> [C.exp| void* {PyArray_DATA($(void* ptr'))} |]
    FC.newForeignPtr (dataPtr) (withGIL $ pyDecRef arr)
  where
    withGIL = runInBoundThread . bracket pyGILStateEnsure pyGILStateRelease . const

npArraySimpleNewFromData :: CInt -> Ptr CLong -> CInt -> Ptr () -> IO PyObject
npArraySimpleNewFromData nd dims typenum dataPtr = do
  arrP :: Ptr PyObject 
    <- castPtr <$> [C.exp| void* {PyArray_SimpleNewFromData($(int nd),
                                                            $(long* dims),
                                                            $(int typenum),
                                                            $(void* dataPtr))} |]
  PyObject <$> newForeignPtr decref_with_gil arrP
