#include <stdint.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#define MAX_DIMS 32

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
    // pointer to the first element
    uint8_t *dataptr;
} NDArray;

typedef struct {
    int nd_m1;
    intptr_t index, length;
    intptr_t coords[MAX_DIMS];
    intptr_t dims_m1[MAX_DIMS];
    intptr_t strides[MAX_DIMS];
    intptr_t backstrides[MAX_DIMS];
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
    NDArray **nda;
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
#define ITER_DATA(it, type) (*((type)it->cursor))
#define ITER_LVAL(it) *(it->cursor)

/* function API */
NDArray *ndarray_new(int n, intptr_t *dims, intptr_t elem_bytes, uint8_t *ptr);
void ndarray_free(NDArray *nda);
NDArrayIter *ndarray_iter_new(NDArray *nda, Slice *slices);
NDArrayIter *ndarray_iter_new_all_but_axis(NDArray *nda, Slice *slices, int *dim);
void ndarray_iter_free(NDArrayIter *it);
bool ndarray_iter_next(NDArrayIter *it);
void ndarray_iter_reset(NDArrayIter *it);
int ndarray_iter_write_file(NDArrayIter *it, FILE *out);
NDArrayMultiIter *ndarray_multi_iter_new(int num, ...);
bool ndarray_multi_iter_next(NDArrayMultiIter *mit);
