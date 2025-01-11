#include <stdint.h>
#include <stdarg.h>
#include <malloc.h>
#include <string.h>

#include "ndarray.h"

/*
  allocates and returns an NDArray the same size as the input multi iterator with 
  element size elem_bytes.
*/
NDArray *ndarray_new_result(NDArrayMultiIter *mit, intptr_t elem_bytes)
{
    uint8_t *c_data = (uint8_t*) malloc(mit->length * elem_bytes);

    intptr_t dims[MAX_DIMS] = {0};
    for(int i = 0; i <= mit->nd_m1; i++)
    {
	dims[i] = mit->dims_m1[i] + 1;
    }
    
    return ndarray_new(mit->nd_m1 + 1, dims, elem_bytes, c_data);
}

// assume arrays contain doubles for now
/* 
   allocates an NDArray to store the result and returns a pointer to it
*/
NDArray *ndarray_mul(NDArray *a, NDArray *b)
{
    NDArrayMultiIter *mit = ndarray_multi_iter_new(2, a, b);

    if(!mit)
    {
	return NULL;
    }

    // todo: add assert to check A and B elem_bytes are equal to sizeof(double)
    
    // allocate result array
    NDArray *c = ndarray_new_result(mit, sizeof(double));
    if(!c)
    {
	ndarray_multi_iter_free(mit);
	return NULL;
    }

    double *result = (double *) NDARRAY_DATAPTR(c);
    do
    {
	*result++ = MULTI_ITER_DATA(mit, 0, double) * MULTI_ITER_DATA(mit, 1, double);
    } while(ndarray_multi_iter_next(mit));

    ndarray_multi_iter_free(mit);
    
    return c;
}
