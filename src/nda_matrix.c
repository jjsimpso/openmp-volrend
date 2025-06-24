#include <stdint.h>
#include <stdarg.h>
#include <malloc.h>
#include <string.h>
#include <math.h>
#include <omp.h>

#include "ndarray.h"
#include "nda_ops.h"

/* 
   NDarrays are stored in row-major order (which is the normal layout
   for C arrays).  However, the dims array starts from the highest
   dimension to the lowest, so dims[0] is the number of rows and
   dims[1] is the number of colums in a 2D matrix.  Hovewer in a row
   vector, dims[0] is the number of columns/elements. Access row i and
   column j with data[i][j], assuming data is the dataptr cast to the
   correct type.
*/

/* calculate the 1d index of element at i(row),j(column) in matrix with cols columns */
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
/*
NDArray *ndarray_matmul_int64_t(NDArray *a, NDArray *b)
{
    if((a->ndim != 2) || (b->ndim != 2) || (a->dims[1] != b->dims[0]))
    {
	return ((void *)0);
    }
    if((a->num_elems > 10000))
    {
	return ndarray_matmul_int64_t_mp(a, b);
    }

    NDArray *c = ndarray_new(a->ndim, (intptr_t []){a->dims[0], b->dims[1]}, a->elem_bytes, ((void *)0));

    if(!c)
    {
	return ((void *)0);
    }
    ndarray_fill_int64_t(c, 0.0);

    intptr_t a_cols = a->dims[1];
    intptr_t b_cols = b->dims[1];
    intptr_t c_cols = c->dims[1];
    intptr_t c_rows = c->dims[0];
    int64_t *adata = (int64_t *)(a->dataptr);
    int64_t *bdata = (int64_t *)(b->dataptr);
    int64_t *cdata = (int64_t *)(c->dataptr);
    
    for (intptr_t i = 0; i < c_rows; i++)
    {
	for (intptr_t k = 0; k < a_cols; k++)
	{
	    for (intptr_t j = 0; j < c_cols; j++)
	    {
		cdata[((j) + ((i) * (c_cols)))] += adata[((k) + ((i) * (a_cols)))] * bdata[((j) + ((k) * (b_cols)))];
	    }
	}
    } return c;
}
*/
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

/*
  matrix multiplication over iterators uses a custom iteration strategy and cannot use ndarray_iter_next().
 */
#define MAKE_NDARRAY_ITER_MATMUL_FUNC(type)                                                                      \
NDArray *ndarray_iter_matmul_##type(NDArrayIter *a, NDArrayIter *b)                                              \
{                                                                                                                \
    if((a->nd_m1 != 1) ||                                                                                        \
       (b->nd_m1 != 1) ||                                                                                        \
       (a->dims_m1[1] != b->dims_m1[0]))                                                                         \
    {                                                                                                            \
	return NULL;                                                                                             \
    }                                                                                                            \
                                                                                                                 \
    intptr_t a_cols = a->dims_m1[1] + 1;                                                                         \
    intptr_t a_rows = a->dims_m1[0] + 1;                                                                         \
    intptr_t b_cols = b->dims_m1[1] + 1;                                                                         \
                                                                                                                 \
    /* no broadcasting for matmul for now */                                                                     \
    /* allocate result array */                                                                                  \
    NDArray *c = ndarray_new(2, (intptr_t []){a_rows, b_cols}, sizeof(type), NULL);                              \
    if(!c)                                                                                                       \
    {                                                                                                            \
	return NULL;                                                                                             \
    }                                                                                                            \
                                                                                                                 \
    intptr_t c_cols = c->dims[1];                                                                                \
    intptr_t c_rows = c->dims[0];                                                                                \
                                                                                                                 \
    ndarray_fill_##type(c, (type)0);                                                                             \
                                                                                                                 \
    type *cdata = (type *)NDARRAY_DATAPTR(c);                                                                    \
                                                                                                                 \
    for (intptr_t i = 0; i < c_rows; i++)                                                                        \
    {                                                                                                            \
	for (intptr_t k = 0; k < a_cols; k++)                                                                    \
        {                                                                                                        \
	    for (intptr_t j = 0; j < c_cols; j++)                                                                \
            {                                                                                                    \
                /*cdata[ELEMENT(i, j, c_cols)] += adata[ELEMENT(i, k, a_cols)] * bdata[ELEMENT(k, j, b_cols)];*/ \
                cdata[ELEMENT(i, j, c_cols)] += ITER_DATA(a, type) * ITER_DATA(b, type);                         \
		if(j < b->dims_m1[1]) b->cursor += b->strides[1];                                                \
            }                                                                                                    \
	    if(k < a->dims_m1[1]) a->cursor += a->strides[1];                                                    \
	    if(k < b->dims_m1[0]) b->cursor -= b->backstrides[1];                                                \
	    if(k < b->dims_m1[0]) b->cursor += b->strides[0];                                                    \
        }                                                                                                        \
	if(i < a->dims_m1[0]) a->cursor -= a->backstrides[1];                                                    \
	if(i < a->dims_m1[0]) a->cursor += a->strides[0];                                                        \
        ndarray_iter_reset(b);                                                                                   \
    }                                                                                                            \
                                                                                                                 \
    /* reset these iterators because they are not in a valid state */                                            \
    ndarray_iter_reset(a);                                                                                       \
    ndarray_iter_reset(b);                                                                                       \
                                                                                                                 \
    return c;                                                                                                    \
}

MAKE_NDARRAY_ITER_MATMUL_FUNC(float)
MAKE_NDARRAY_ITER_MATMUL_FUNC(double)
MAKE_NDARRAY_ITER_MATMUL_FUNC(int8_t)
MAKE_NDARRAY_ITER_MATMUL_FUNC(int16_t)
MAKE_NDARRAY_ITER_MATMUL_FUNC(int32_t)
MAKE_NDARRAY_ITER_MATMUL_FUNC(int64_t)
MAKE_NDARRAY_ITER_MATMUL_FUNC(uint8_t)
MAKE_NDARRAY_ITER_MATMUL_FUNC(uint16_t)
MAKE_NDARRAY_ITER_MATMUL_FUNC(uint32_t)
MAKE_NDARRAY_ITER_MATMUL_FUNC(uint64_t)

/* sample expansion
NDArray *ndarray_iter_matmul_int64_t(NDArrayIter *a, NDArrayIter *b)
{
    if((a->nd_m1 != 1) || (b->nd_m1 != 1) || (a->dims_m1[1] != b->dims_m1[0]))
    {
	return ((void *)0);
    }

    intptr_t a_cols = a->dims_m1[1] + 1;
    intptr_t a_rows = a->dims_m1[0] + 1;
    intptr_t b_cols = b->dims_m1[1] + 1;
    NDArray *c = ndarray_new(2, (intptr_t []){a_rows, b_cols}, sizeof(int64_t), ((void *)0));
    if(!c)
    {
	return ((void *)0);
    }

    intptr_t c_cols = c->dims[1];
    intptr_t c_rows = c->dims[0];

    ndarray_fill_int64_t(c, (int64_t)0);

    int64_t *cdata = (int64_t *)(c->dataptr);

    for (intptr_t i = 0; i < c_rows; i++)
    {
	for (intptr_t k = 0; k < a_cols; k++)
	{
	    for (intptr_t j = 0; j < c_cols; j++)
	    {
		cdata[((j) + ((i) * (c_cols)))] += (*((int64_t *)a->cursor)) * (*((int64_t *)b->cursor));
		if(j < b->dims_m1[1]) b->cursor += b->strides[1];
	    }
	    if(k < a->dims_m1[1]) a->cursor += a->strides[1];
	    if(k < b->dims_m1[0]) b->cursor -= b->backstrides[1];
	    if(k < b->dims_m1[0]) b->cursor += b->strides[0];
	}
	if(i < a->dims_m1[0]) a->cursor -= a->backstrides[1];
	if(i < a->dims_m1[0]) a->cursor += a->strides[0];
	ndarray_iter_reset(b);
    }
    
    ndarray_iter_reset(a);
    ndarray_iter_reset(b);
    return c;
}
*/
