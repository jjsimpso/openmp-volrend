#include <stdint.h>
#include <stdarg.h>
#include <malloc.h>
#include <string.h>

#include "ndarray.h"

// assume arrays contain doubles
NDArray *ndarray_mul(NDArray *a, NDArray *b)
{
    NDArrayMultiIter *mit = ndarray_multi_iter_new(2, a, b);

    if(!mit)
    {
	return NULL;
    }

    // todo: add assert to check A and B elem_bytes are equal to sizeof(double)
    intptr_t elem_bytes = sizeof(double);
    uint8_t *c_data = (uint8_t*) malloc(mit->length * elem_bytes);

    intptr_t dims[MAX_DIMS];
    memcpy(dims, mit->dims_m1, sizeof(dims));
    for(int i = 0; i <= mit->nd_m1; i++)
    {
	dims[i]++;
    }
    
    NDArray *c = ndarray_new(mit->nd_m1 + 1, dims, elem_bytes, c_data);
    if(!c)
    {
	ndarray_multi_iter_free(mit);
	return NULL;
    }

    double *result = (double *) c_data;
    double *ap, *bp;
    do
    {
	ap = (double *)MULTI_ITER_DATAPTR(mit, 0);
	bp = (double *)MULTI_ITER_DATAPTR(mit, 1);
	*result = *ap * *bp;
	//*result = MULTI_ITER_DATA(mit, 0, double) * MULTI_ITER_DATA(mit, 1, double);
	result += 1;
    } while(ndarray_multi_iter_next(mit));
    
    return c;
}
