#ifndef HS_SKLEARN_GLUE_H
#define HS_SKLEARN_GLUE_H

#include <glib.h>

extern GAsyncQueue* globalDecRefQueue;

void decref_with_gil(PyObject* obj);
void queue_decref(PyObject* obj);
GAsyncQueue* get_decref_queue();
void decref_check_count(PyObject* obj);

#endif  /* HS_SKLEARN_GLUE_H */
