
#include <Python.h>
#include <stdio.h>
#include "glue.h"

void decref_with_gil(PyObject* obj) {
  PyGILState_STATE gstate;
  int have_gil;
  have_gil = PyGILState_Check();
  fprintf(stderr, "decref_with_gil requesting gil (currently have it? %d)\n", have_gil);
  gstate = PyGILState_Ensure();
  Py_DECREF(obj);
  if(!_Py_IsFinalizing())
    PyGILState_Release(gstate);
}
