#include <stdint.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#pragma once

#define MAX_DIMS 32

#define OPENMP_ELEM_THRESHOLD 10000

typedef struct {
    // number of dimensions, N
    int ndim;
    // array containing number of elements in each dimension
    intptr_t *dims;
    // total number of elements in the array
    intptr_t num_elems;
    // size of each element in bytes
    intptr_t elem_bytes;
    // total size of the array in bytes
    intptr_t size;
    // flag to indicate that dataptr should be free'd
    bool free_data;
    // pointer to the first element
    uint8_t *dataptr;
} NDArray;

typedef struct {
    // number of dimensions minus one
    int nd_m1;
    // current index of the iterator and total number of indices in iterator
    intptr_t index, length;
    // coordinates of current index/position in iterator
    intptr_t coords[MAX_DIMS];
    // number of elements of each dimension minus one 
    intptr_t dims_m1[MAX_DIMS];
    // stride and backstride of each dimension
    intptr_t strides[MAX_DIMS];
    intptr_t backstrides[MAX_DIMS];
    // starting index in nda of each dimension
    intptr_t slicestarts[MAX_DIMS];
    NDArray *nda;
    uint8_t *cursor;
    bool contiguous;
} NDArrayIter;

typedef struct {
    int num;
    int nd_m1;
    intptr_t index, length;
    intptr_t dims_m1[MAX_DIMS];
    // pointers to the nd arrays used to initialize the multi iterator
    NDArray **nda;
    // pointers to the iterators
    NDArrayIter **iter;
} NDArrayMultiIter;

typedef struct {
    intptr_t start, end, stride; // specified in elements, not bytes
} Slice;

/* macro API */
#define NDARRAY_N(nda) (nda->ndim)
#define NDARRAY_DIM_SIZE(nda, dim) (nda->dims[dim])
#define NDARRAY_DATAPTR(nda) (nda->dataptr)

#define ITER_DATAPTR(it) (it->cursor)
#define ITER_DATA(it, type) (*((type *)it->cursor))
#define ITER_LVAL(it, type) *((type *)it->cursor)

#define MULTI_ITER_DATAPTR(mit, n) (mit->iter[n]->cursor)
#define MULTI_ITER_DATA(mit, n, type) (*((type *)mit->iter[n]->cursor))

/* function API */
NDArray *ndarray_new(int n, intptr_t *dims, intptr_t elem_bytes, uint8_t *ptr);
void ndarray_set_freedata(NDArray *nda, bool freedata);
void ndarray_free(NDArray *nda);
NDArray *ndarray_copy(NDArray *nda);
NDArrayIter *ndarray_iter_new(NDArray *nda, Slice *slices);
NDArrayIter *ndarray_iter_new_all_but_axis(NDArray *nda, Slice *slices, int *dim);
NDArrayIter *ndarray_iter_new_add_axis(NDArray *nda, Slice *slices, int axis);
void ndarray_iter_free(NDArrayIter *it);
bool ndarray_iter_next(NDArrayIter *it);
void ndarray_iter_reset(NDArrayIter *it);
NDArrayIter *ndarray_iter_copy(NDArrayIter *iter);
int ndarray_iter_write_file(NDArrayIter *it, FILE *out);
NDArrayMultiIter *ndarray_multi_iter_new(int num, ...);
NDArrayMultiIter *ndarray_multi_iter_new_from_iter(int num, ...);
void ndarray_multi_iter_free(NDArrayMultiIter *mit);
void ndarray_multi_iter_free_except_iter(NDArrayMultiIter *mit);
bool ndarray_multi_iter_next(NDArrayMultiIter *mit);

bool ndarray_shape_equal(NDArray *a, NDArray *b);
bool ndarray_data_equal(NDArray *a, NDArray *b);
