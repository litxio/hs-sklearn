
#include <Python.h>
#include "glue.h"

void decref_with_gil(PyObject* obj) {
  PyGILState_STATE gstate;
  gstate = PyGILState_Ensure();
  Py_DECREF(obj);
  PyGILState_Release(gstate);
}
