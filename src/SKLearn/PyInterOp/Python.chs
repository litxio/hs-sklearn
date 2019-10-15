
#include <Python.h>
#include <glib.h>
#include "glue.h"

module SKLearn.PyInterOp.Python where

import Data.Int
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String

{#pointer *PyObject as PyObject foreign finalizer queue_decref as py_decref newtype#}
{#pointer *GAsyncQueue as GAsyncQueue#}

peekCWStringCast = peekCWString . castPtr

peekDouble :: Ptr (Ptr PyObject) -> IO PyObject
peekDouble pp = do
  p <- peek pp
  PyObject <$> newForeignPtr py_decref p

-- {#fun Py_Main as ^ {`Int', `[String]' peek} -> `()' #}
{#fun Py_Initialize as ^ {} -> `()' #}
{#fun Py_Finalize as ^ {} -> `()' #}
{#fun Py_GetVersion as ^ {} -> `String' #}
{#fun Py_GetPath as ^ {} -> `String' peekCWStringCast* #}
{#fun Py_DecRef as ^ {`PyObject'} -> `()' #}
{#fun Py_DecRef as pyDecRefRawPtr {id `Ptr PyObject' id-} -> `()' #}
{#fun Py_IncRef as ^ {`PyObject'} -> `()' #}

{#fun PyGILState_Ensure as ^ {} -> `Int' #}
{#fun PyGILState_Release as ^ {`Int'} -> `()' #}


{#fun PyUnicode_AsUTF8 as ^ {`PyObject'} -> `String' #}
{#fun PyUnicode_DecodeLocale as ^ {`String', `String'} -> `PyObject' #}

{#fun PyTuple_New as ^ {`Int'} -> `PyObject' #}
-- NOTE: PyTuple_SetItem steals the reference to the third argument, so we
-- need to call Py_IncRef if we want to keep using it!
{#fun PyTuple_SetItem as ^ {`PyObject', `Int', `PyObject'} -> `Int' #}

{#fun PyImport_ImportModule as ^ {`String'} -> `PyObject' #}

{#fun PyObject_CallObject as ^ {`PyObject', `PyObject'} -> `PyObject' #}
{#fun PyObject_GetAttrString as ^ {`PyObject', `String'} -> `PyObject' #}

{#fun PyLong_FromLong as ^ {`Int64'} -> `PyObject' #}

{#fun PyFloat_AsDouble as ^ {`PyObject'} -> `Double' #}

{#fun PyErr_PrintEx as ^ {`Int'} -> `()' #}
{#fun PyErr_Fetch as ^ {alloca- `PyObject' peekDouble*,
                        alloca- `PyObject' peekDouble*, 
                        alloca- `PyObject' peekDouble*} -> `()' #}

{#fun PyErr_Occurred as ^ {} -> `PyObject' #}

-- A couple of helper functions from glib
{#fun g_async_queue_new as ^ {} -> `GAsyncQueue' #}
{#fun g_async_queue_push as ^ {`GAsyncQueue', `Ptr ()'} -> `()' #}
{#fun g_async_queue_pop as ^ {`GAsyncQueue'} -> `Ptr ()' #}
{#fun g_async_queue_timeout_pop as ^ {`GAsyncQueue', `Int64'} -> `Ptr ()' #}
{#fun g_async_queue_length as ^ {`GAsyncQueue'} -> `Int' #}

{#fun queue_decref as ^ {`PyObject'} -> `()' #}
{#fun get_decref_queue as ^ {} -> `GAsyncQueue' #}


-- foreign import ccall "&globalDecRefQueue" globalDecRefQueue :: GAsyncQueue
