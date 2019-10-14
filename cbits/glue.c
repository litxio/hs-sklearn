
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


void decref_check_count(PyObject* obj) {
  // int refcnt;
  // int have_gil;
  // PyGILState_STATE gstate;
  // refcnt = obj -> ob_refcnt;
  // have_gil = PyGILState_Check();
  // gstate = PyGILState_Ensure();
  // fprintf(stderr, "decref with %d current references; have gil is %d\n", refcnt, have_gil);
  // Py_DECREF(obj);
  // if(!_Py_IsFinalizing())
  //   PyGILState_Release(gstate);
}
