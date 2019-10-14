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
import Debug.Trace as Debug
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


initNumpy :: IO ()
initNumpy = [C.block| void { import_array(); } |]


npArraySimpleNew :: CInt -> Ptr CLong -> CInt -> IO PyObject
npArraySimpleNew nd dims typenum = do
  arrP :: Ptr PyObject
    <- castPtr <$> [C.exp| void* {PyArray_SimpleNew($(int nd), $(long* dims), $(int typenum))} |]
  PyObject <$> newForeignPtr py_decref arrP
     

-- | This gives us a weird sort of dependency where we want the original
-- PyObject to NOT get its reference count decreased until we are done with 
-- the resulting array.  
npArrayData :: PyObject -> IO (ForeignPtr a)
npArrayData arr@(PyObject fptr) =
  withForeignPtr fptr $ \ptr -> do
    let ptr' = castPtr ptr
    pyIncRef arr
    dataPtr <- castPtr <$> [C.exp| void* {PyArray_DATA($(void* ptr'))} |]
    FC.newForeignPtr (dataPtr) (pyDecRef arr)


npArraySimpleNewFromData :: CInt -> Ptr CLong -> CInt -> Ptr () -> IO PyObject
npArraySimpleNewFromData nd dims typenum dataPtr = do
  arrP :: Ptr PyObject 
    <- castPtr <$> [C.exp| void* {PyArray_SimpleNewFromData($(int nd),
                                                            $(long* dims),
                                                            $(int typenum),
                                                            $(void* dataPtr))} |]
  PyObject <$> newForeignPtr py_decref arrP
