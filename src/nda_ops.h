#include "ndarray.h"

/* function API */
void ndarray_fill(NDArray *a, double val);
NDArray *ndarray_mul(NDArray *a, NDArray *b);
NDArray *ndarray_iter_mul(NDArrayIter *a, NDArrayIter *b);
