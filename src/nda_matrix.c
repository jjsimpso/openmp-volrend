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

#define MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(type)         \
bool ndarray_fill_mat_ident_##type(NDArray *a)         \
{                                                      \
    /* must be a square matrix */                      \
    if(a->dims[0] != a->dims[1])                       \
    {                                                  \
	return false;                                  \
    }                                                  \
                                                       \
    intptr_t a_cols = a->dims[1];                      \
    type *cursor = (type *)NDARRAY_DATAPTR(a);         \
                                                       \
    ndarray_fill_##type(a, 0.0);                       \
                                                       \
    for(intptr_t ij = 0; ij < a->dims[0]; ij++)        \
    {                                                  \
	cursor[ELEMENT(ij, ij, a_cols)] = 1.0;         \
    }                                                  \
                                                       \
    return true;                                       \
}

MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(float)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(double)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(int8_t)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(int16_t)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(int32_t)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(int64_t)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(uint8_t)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(uint16_t)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(uint32_t)
MAKE_NDARRAY_FILL_MAT_IDENT_FUNC(uint64_t)

/* sample expansion
bool ndarray_fill_mat_ident_double(NDArray *a)
{
    // must be a square matrix
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
*/

#define MAKE_NDARRAY_MATMUL_MP_FUNC(type)                                                                         \
NDArray *ndarray_matmul_##type##_mp(NDArray *a, NDArray *b)                                                       \
{                                                                                                                 \
    /* allocate result array */                                                                                   \
    NDArray *c = ndarray_new(a->ndim, (intptr_t []){a->dims[0], b->dims[1]}, a->elem_bytes, NULL);                \
    if(!c)                                                                                                        \
    {                                                                                                             \
	return NULL;                                                                                              \
    }                                                                                                             \
                                                                                                                  \
    ndarray_fill_##type(c, 0.0);                                                                                  \
                                                                                                                  \
    intptr_t a_cols = a->dims[1];                                                                                 \
    intptr_t b_cols = b->dims[1];                                                                                 \
    intptr_t c_cols = c->dims[1];                                                                                 \
    intptr_t c_rows = c->dims[0];                                                                                 \
    type *adata = (type *)NDARRAY_DATAPTR(a);                                                                     \
    type *bdata = (type *)NDARRAY_DATAPTR(b);                                                                     \
    type *cdata = (type *)NDARRAY_DATAPTR(c);                                                                     \
                                                                                                                  \
    _Pragma("omp parallel for")                                                                                   \
    for (intptr_t i = 0; i < c_rows; i++)                                                                         \
    {                                                                                                             \
	for (intptr_t k = 0; k < a_cols; k++)                                                                     \
        {                                                                                                         \
	    for (intptr_t j = 0; j < c_cols; j++)                                                                 \
            {                                                                                                     \
                cdata[ELEMENT(i, j, c_cols)] += adata[ELEMENT(i, k, a_cols)] * bdata[ELEMENT(k, j, b_cols)];      \
            }                                                                                                     \
        }                                                                                                         \
    }                                                                                                             \
                                                                                                                  \
    return c;                                                                                                     \
}

MAKE_NDARRAY_MATMUL_MP_FUNC(float)
MAKE_NDARRAY_MATMUL_MP_FUNC(double)
MAKE_NDARRAY_MATMUL_MP_FUNC(int8_t)
MAKE_NDARRAY_MATMUL_MP_FUNC(int16_t)
MAKE_NDARRAY_MATMUL_MP_FUNC(int32_t)
MAKE_NDARRAY_MATMUL_MP_FUNC(int64_t)
MAKE_NDARRAY_MATMUL_MP_FUNC(uint8_t)
MAKE_NDARRAY_MATMUL_MP_FUNC(uint16_t)
MAKE_NDARRAY_MATMUL_MP_FUNC(uint32_t)
MAKE_NDARRAY_MATMUL_MP_FUNC(uint64_t)

/*
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
*/

#define MAKE_NDARRAY_MATMUL_FUNC(type)                                                                           \
NDArray *ndarray_matmul_##type(NDArray *a, NDArray *b)                                                           \
{                                                                                                                \
    if((a->ndim != 2) ||                                                                                         \
       (b->ndim != 2) ||                                                                                         \
       (a->dims[1] != b->dims[0]))                                                                               \
    {                                                                                                            \
	return NULL;                                                                                             \
    }                                                                                                            \
                                                                                                                 \
    if((a->num_elems > OPENMP_ELEM_THRESHOLD))                                                                   \
    {                                                                                                            \
        return ndarray_matmul_##type##_mp(a, b);                                                                 \
    }                                                                                                            \
                                                                                                                 \
    /* no broadcasting for matmul for now */                                                                     \
    /* allocate result array */                                                                                  \
    NDArray *c = ndarray_new(a->ndim, (intptr_t []){a->dims[0], b->dims[1]}, a->elem_bytes, NULL);               \
    if(!c)                                                                                                       \
    {                                                                                                            \
	return NULL;                                                                                             \
    }                                                                                                            \
                                                                                                                 \
    ndarray_fill_##type(c, 0.0);                                                                                 \
                                                                                                                 \
    intptr_t a_cols = a->dims[1];                                                                                \
    intptr_t b_cols = b->dims[1];                                                                                \
    intptr_t c_cols = c->dims[1];                                                                                \
    intptr_t c_rows = c->dims[0];                                                                                \
    type *adata = (type *)NDARRAY_DATAPTR(a);                                                                    \
    type *bdata = (type *)NDARRAY_DATAPTR(b);                                                                    \
    type *cdata = (type *)NDARRAY_DATAPTR(c);                                                                    \
                                                                                                                 \
    for (intptr_t i = 0; i < c_rows; i++)                                                                        \
    {                                                                                                            \
	for (intptr_t k = 0; k < a_cols; k++)                                                                    \
        {                                                                                                        \
	    for (intptr_t j = 0; j < c_cols; j++)                                                                \
            {                                                                                                    \
                cdata[ELEMENT(i, j, c_cols)] += adata[ELEMENT(i, k, a_cols)] * bdata[ELEMENT(k, j, b_cols)];     \
            }                                                                                                    \
        }                                                                                                        \
    }                                                                                                            \
                                                                                                                 \
    return c;                                                                                                    \
}

MAKE_NDARRAY_MATMUL_FUNC(float)
MAKE_NDARRAY_MATMUL_FUNC(double)
MAKE_NDARRAY_MATMUL_FUNC(int8_t)
MAKE_NDARRAY_MATMUL_FUNC(int16_t)
MAKE_NDARRAY_MATMUL_FUNC(int32_t)
MAKE_NDARRAY_MATMUL_FUNC(int64_t)
MAKE_NDARRAY_MATMUL_FUNC(uint8_t)
MAKE_NDARRAY_MATMUL_FUNC(uint16_t)
MAKE_NDARRAY_MATMUL_FUNC(uint32_t)
MAKE_NDARRAY_MATMUL_FUNC(uint64_t)

/* sample expansion
NDArray *ndarray_matmul_double(NDArray *a, NDArray *b)
{
    if((a->ndim != 2) ||
       (b->ndim != 2) ||
       (a->dims[1] != b->dims[0]))
    {
	return NULL;
    }
    
    if((a->num_elems > OPENMP_ELEM_THRESHOLD))
    {
        return ndarray_matmul_double_mp(a, b);
    }

    // no broadcasting for matmul for now
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
*/
