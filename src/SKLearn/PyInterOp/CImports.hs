{-# LANGUAGE ForeignFunctionInterface, ExtendedDefaultRules, GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SKLearn.PyInterOp.CImports where

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

data PyObject = PyObject
type PyObjectPtr = Ptr PyObject

C.include "/usr/lib/python3.7/site-packages/numpy/core/include/numpy/arrayobject.h"

npDoubleType = [C.pure| int{NPY_DOUBLE} |]

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
foreign import ccall "Py_IncRef" pyIncRef :: PyObjectPtr -> IO ()
foreign import ccall "&Py_DecRef" pyDecRef_Ptr :: FunPtr (Ptr a -> IO ())

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

initNumpy :: IO ()
initNumpy = [C.block| void { import_array(); return; } |]

npArraySimpleNew :: CInt -> Ptr CLong -> CInt -> IO PyObjectPtr
npArraySimpleNew nd dims typenum 
  = castPtr <$> [C.exp| void* {PyArray_SimpleNew($(int nd), $(long* dims), $(int typenum))} |]

npArrayData :: PyObjectPtr -> IO (Ptr a)
npArrayData arr = do
  let arr' = castPtr arr
  pyIncRef arr 
  castPtr <$> [C.exp| void* {PyArray_DATA($(void* arr'))} |]

npArraySimpleNewFromData :: CInt -> Ptr CLong -> CInt -> Ptr () -> IO PyObjectPtr
npArraySimpleNewFromData nd dims typenum dataPtr
  = castPtr <$> [C.exp| void* {PyArray_SimpleNewFromData($(int nd),
                                             $(long* dims),
                                             $(int typenum),
                                             $(void* dataPtr))} |]
