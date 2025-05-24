#include <stdint.h>
#include <stdarg.h>
#include <malloc.h>
#include <string.h>
#include <math.h>
#include <omp.h>

#include "ndarray.h"
#include "nda_ops.h"

/* calculate the 1d index of element at i,j in matrix with cols columns */
#define ELEMENT(i, j, cols) ((j) + ((i) * (cols)))

bool ndarray_fill_mat_ident_double(NDArray *a)
{
    /* must be a square matrix */
    if(a->dims[0] != a->dims[1])
    {
	return false;
    }
    
    intptr_t a_cols = a->dims[1];
    double *cursor = (double *)NDARRAY_DATAPTR(a);

    ndarray_fill_double(a, 0.0);
    
    for(intptr_t ij = 0; ij < a->dims[0]; ij++)
    {
	cursor[ELEMENT(ij, ij, a_cols)] = 1.0;
    }

    return true;
}

NDArray *ndarray_matmul_double_mp(NDArray *a, NDArray *b)
{
    // allocate result array
    NDArray *c = ndarray_new(a->ndim, (intptr_t []){a->dims[0], b->dims[1]}, a->elem_bytes, NULL);
    if(!c)
    {
	return NULL;
    }

    ndarray_fill_double(c, 0.0);
    
    intptr_t a_cols = a->dims[1];
    intptr_t b_cols = b->dims[1];
    intptr_t c_cols = c->dims[1];
    intptr_t c_rows = c->dims[0];
    double *adata = (double *)NDARRAY_DATAPTR(a);
    double *bdata = (double *)NDARRAY_DATAPTR(b);
    double *cdata = (double *)NDARRAY_DATAPTR(c);

    #pragma omp parallel for
    for (intptr_t i = 0; i < c_rows; i++)
    {
	for (intptr_t k = 0; k < a_cols; k++)
        {
	    for (intptr_t j = 0; j < c_cols; j++)
            {
                cdata[ELEMENT(i, j, c_cols)] += adata[ELEMENT(i, k, a_cols)] * bdata[ELEMENT(k, j, b_cols)];
            }
        }
    }

    return c;
}
