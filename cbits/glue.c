
#include <Python.h>
#include <stdio.h>
#include <glib.h>
#include "glue.h"

GAsyncQueue* globalDecRefQueue = 0L;

GAsyncQueue* get_decref_queue() {
  if(globalDecRefQueue == 0) {
    globalDecRefQueue = g_async_queue_new();
  }
  return globalDecRefQueue;
}

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

void queue_decref(PyObject* obj) {
  int queue_len;
  if(globalDecRefQueue == 0) {
    globalDecRefQueue = g_async_queue_new();
  }
  if(obj == 0) {
  //  fprintf(stderr, "queue_decref called on null object!?\n");
    return;
  }// else
    //fprintf(stderr, "queue_decref called on non-null object, whew\n");

  g_async_queue_push(globalDecRefQueue, obj);
  queue_len = g_async_queue_length (globalDecRefQueue);
  // fprintf(stderr, "New queue len %d\n", queue_len);
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
