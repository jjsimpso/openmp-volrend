#include <stdint.h>
#include <stdbool.h>

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
    intptr_t start, end, stride; // specified in elements, not bytes
} Slice;

/* function API */
NDArray *ndarray_new(int n, intptr_t *dims, intptr_t elem_size);
void ndarray_free(NDArray *nda);
NDArrayIter *ndarray_iter_new(NDArray *nda, Slice *slices);
bool ndarray_iter_next(NDArrayIter *it);
void ndarray_iter_reset(NDArrayIter *it);
