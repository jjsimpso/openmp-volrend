#include <stdint.h>
#include <stdarg.h>
#include <malloc.h>
#include <string.h>
#include <math.h>
#include <omp.h>

#include "ndarray.h"

/*
  allocates and returns an NDArray the same size as the input multi iterator with 
  element size elem_bytes.
*/
NDArray *ndarray_new_result(NDArrayMultiIter *mit, intptr_t elem_bytes)
{
    intptr_t dims[MAX_DIMS] = {0};
    for(int i = 0; i <= mit->nd_m1; i++)
    {
	dims[i] = mit->dims_m1[i] + 1;
    }
    
    return ndarray_new(mit->nd_m1 + 1, dims, elem_bytes, NULL);
}

/*
  As above but argument is a standard iterator.
*/
NDArray *ndarray_new_result_single(NDArrayIter *it, intptr_t elem_bytes)
{
    intptr_t dims[MAX_DIMS] = {0};
    for(int i = 0; i <= it->nd_m1; i++)
    {
	dims[i] = it->dims_m1[i] + 1;
    }
    
    return ndarray_new(it->nd_m1 + 1, dims, elem_bytes, NULL);
}

/*

*/
#define MAKE_NDARRAY_FILL_FUNC(type)	             \
void ndarray_fill_##type(NDArray *a, type val)       \
{                                                    \
    type *cursor = (type *)NDARRAY_DATAPTR(a);       \
    for(int i = 0; i < a->num_elems; i++)            \
    {                                                \
	*cursor++ = val;                             \
    }                                                \
}

MAKE_NDARRAY_FILL_FUNC(float)
MAKE_NDARRAY_FILL_FUNC(double)
MAKE_NDARRAY_FILL_FUNC(int8_t)
MAKE_NDARRAY_FILL_FUNC(int16_t)
MAKE_NDARRAY_FILL_FUNC(int32_t)
MAKE_NDARRAY_FILL_FUNC(int64_t)
MAKE_NDARRAY_FILL_FUNC(uint8_t)
MAKE_NDARRAY_FILL_FUNC(uint16_t)
MAKE_NDARRAY_FILL_FUNC(uint32_t)
MAKE_NDARRAY_FILL_FUNC(uint64_t)

/*

*/
#define MAKE_NDARRAY_FILL_INDEX_FUNC(type)           \
void ndarray_fill_index_##type(NDArray *a)           \
{                                                    \
    type *cursor = (type *)NDARRAY_DATAPTR(a);       \
    for(int i = 0; i < a->num_elems; i++)            \
    {                                                \
	*cursor++ = (type)i;			     \
    }                                                \
}

MAKE_NDARRAY_FILL_INDEX_FUNC(float)
MAKE_NDARRAY_FILL_INDEX_FUNC(double)
MAKE_NDARRAY_FILL_INDEX_FUNC(int8_t)
MAKE_NDARRAY_FILL_INDEX_FUNC(int16_t)
MAKE_NDARRAY_FILL_INDEX_FUNC(int32_t)
MAKE_NDARRAY_FILL_INDEX_FUNC(int64_t)
MAKE_NDARRAY_FILL_INDEX_FUNC(uint8_t)
MAKE_NDARRAY_FILL_INDEX_FUNC(uint16_t)
MAKE_NDARRAY_FILL_INDEX_FUNC(uint32_t)
MAKE_NDARRAY_FILL_INDEX_FUNC(uint64_t)

#define MAKE_NDARRAY_SUM_FUNC(type)                                  \
type ndarray_sum_##type(NDArray *a)                                  \
{                                                                    \
    type sum = 0.0;                                                  \
    type *data = (type *)NDARRAY_DATAPTR(a);                         \
                                                                     \
    if(a->num_elems > OPENMP_ELEM_THRESHOLD)                         \
    {                                                                \
        _Pragma("omp parallel for shared(data) reduction(+:sum)")    \
	for(int i = 0; i < a->num_elems; i++)                        \
	{                                                            \
	    sum += data[i];                                          \
	}                                                            \
    }                                                                \
    else                                                             \
    {	                                                             \
	for(int i = 0; i < a->num_elems; i++)                        \
	{                                                            \
	    sum += data[i];                                          \
	}                                                            \
    }                                                                \
                                                                     \
    return sum;                                                      \
}

MAKE_NDARRAY_SUM_FUNC(float)
MAKE_NDARRAY_SUM_FUNC(double)
MAKE_NDARRAY_SUM_FUNC(int32_t)
MAKE_NDARRAY_SUM_FUNC(int64_t)
MAKE_NDARRAY_SUM_FUNC(uint32_t)
MAKE_NDARRAY_SUM_FUNC(uint64_t)

/* sample expansion
double ndarray_sum_double(NDArray *a)
{
    double sum = 0.0;
    double *data = (double *)NDARRAY_DATAPTR(a);

    if(a->num_elems > OPENMP_ELEM_THRESHOLD)
    {
        #pragma omp parallel for shared(data) reduction(+:sum)
	for(int i = 0; i < a->num_elems; i++)
	{
	    sum += data[i];
	}
    }
    else
    {	
	for(int i = 0; i < a->num_elems; i++)
	{
	    sum += data[i];
	}
    }
    
    return sum;
}
*/

#define MAKE_NDARRAY_ITER_SUM_FUNC(type)                             \
type ndarray_iter_sum_##type(NDArrayIter *a)                         \
{                                                                    \
    type sum = 0.0;                                                  \
        						             \
    do                                                               \
    {                                                                \
	sum += ITER_DATA(a, type);                                   \
    } while(ndarray_iter_next(a));                                   \
                                                                     \
    return sum;                                                      \
}

MAKE_NDARRAY_ITER_SUM_FUNC(float)
MAKE_NDARRAY_ITER_SUM_FUNC(double)
MAKE_NDARRAY_ITER_SUM_FUNC(int32_t)
MAKE_NDARRAY_ITER_SUM_FUNC(int64_t)
MAKE_NDARRAY_ITER_SUM_FUNC(uint32_t)
MAKE_NDARRAY_ITER_SUM_FUNC(uint64_t)

bool ndarray_equal(NDArray *a, NDArray *b)
{
    if((a->ndim != b->ndim) ||
       (a->num_elems != b->num_elems) ||
       (a->size != b->size))
    {
	return false;
    }

    if(memcmp(a->dataptr, b->dataptr, a->size))
    {
	return false;
    }

    return true;
}

#define MAKE_NDARRAY_ITER_EQUAL_FUNC(type)                                 \
bool ndarray_iter_equal_##type(NDArrayIter *a, NDArrayIter *b)	           \
{                                                                          \
    if((a->nd_m1 != b->nd_m1) ||                                           \
       (a->length != b->length) ||                                         \
       (a->nda->elem_bytes != b->nda->elem_bytes))                         \
    {                                                                      \
	return false;                                                      \
    }                                                                      \
                                                                           \
    NDArrayMultiIter *mit = ndarray_multi_iter_new(2, a, b);               \
                                                                           \
    if(!mit)                                                               \
    {                                                                      \
	return false;                                                      \
    }                                                                      \
             						                   \
    do                                                                     \
    {                                                                      \
	if(MULTI_ITER_DATA(mit, 0, type) != MULTI_ITER_DATA(mit, 1, type)) \
	{                                                                  \
	    return false;                                                  \
	}                                                                  \
    } while(ndarray_multi_iter_next(mit));                                 \
                                                                           \
    return true;                                                           \
}

MAKE_NDARRAY_ITER_EQUAL_FUNC(float)
MAKE_NDARRAY_ITER_EQUAL_FUNC(double)
MAKE_NDARRAY_ITER_EQUAL_FUNC(int8_t)
MAKE_NDARRAY_ITER_EQUAL_FUNC(int16_t)
MAKE_NDARRAY_ITER_EQUAL_FUNC(int32_t)
MAKE_NDARRAY_ITER_EQUAL_FUNC(int64_t)
MAKE_NDARRAY_ITER_EQUAL_FUNC(uint8_t)
MAKE_NDARRAY_ITER_EQUAL_FUNC(uint16_t)
MAKE_NDARRAY_ITER_EQUAL_FUNC(uint32_t)
MAKE_NDARRAY_ITER_EQUAL_FUNC(uint64_t)

/* 
   allocates an NDArray to store the result and returns a pointer to it
*/
#define MAKE_NDARRAY_OP_FUNC(name, op, type)	                                                 \
NDArray *ndarray_##name##_##type(NDArray *a, NDArray *b)                                         \
{                                                                                                \
    if((a->num_elems > OPENMP_ELEM_THRESHOLD) && ndarray_shape_equal(a, b))                      \
    {                                                                                            \
        return ndarray_##name##_##type##_mp(a, b);                                               \
    }                                                                                            \
                                                                                                 \
    NDArrayMultiIter *mit = ndarray_multi_iter_new(2, a, b);                                     \
                                                                                                 \
    if(!mit)                                                                                     \
    {                                                                                            \
	return NULL;                                                                             \
    }                                                                                            \
                                                                                                 \
    /* todo: add assert to check A and B elem_bytes are equal to sizeof(type) */                 \
                                                                                                 \
    /* allocate result array */			                                 	         \
    NDArray *c = ndarray_new_result(mit, sizeof(type));                                          \
    if(!c)                                                                                       \
    {                                                                                            \
	ndarray_multi_iter_free(mit);                                                            \
	return NULL;                                                                             \
    }                                                                                            \
                                                                                                 \
    type *result = (type *) NDARRAY_DATAPTR(c);                                                  \
    do                                                                                           \
    {                                                                                            \
	*result++ = MULTI_ITER_DATA(mit, 0, type) op MULTI_ITER_DATA(mit, 1, type);              \
    } while(ndarray_multi_iter_next(mit));                                                       \
                                                                                                 \
    ndarray_multi_iter_free(mit);                                                                \
                                                                                                 \
    return c;                                                                                    \
}

/* sample expansion
NDArray *ndarray_mul_double(NDArray *a, NDArray *b)
{
    if((a->num_elems > OPENMP_ELEM_THRESHOLD) && ndarray_shape_equal(a, b))
    {
        return ndarray_mul_double_mp(a, b);
    }

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
*/

/*
  If the NDArrays have the same shape, we can use an algorithm accelerated by openmp. We
  also don't need to use iterators since we won't be broadcasting and NDArrays are contiguous.
*/
#define MAKE_NDARRAY_OP_MP_FUNC(name, op, type)	                                                 \
NDArray *ndarray_##name##_##type##_mp(NDArray *a, NDArray *b)                                    \
{                                                                                                \
    /* allocate result array */ 	                                                         \
    NDArray *c = ndarray_new(a->ndim, a->dims, a->elem_bytes, NULL);                             \
    if(!c)                                                                                       \
    {                                                                                            \
	return NULL;                                                                             \
    }                                                                                            \
                                                                                                 \
    intptr_t elem_stride = a->elem_bytes;                                                        \
    /* stride of first dimension */                                                              \
    intptr_t base_stride = (a->num_elems * elem_stride) / a->dims[0];                            \
    /* number of elements within each iteration of 1st dimension*/                               \
    intptr_t sub_len = base_stride / elem_stride;                                                \
                                                                                                 \
    _Pragma("omp parallel for")                                                                  \
    for(int i = 0; i < a->dims[0]; i++)                                                          \
    {                                                                                            \
	type *result = (type *)(c->dataptr + (base_stride * i));                                 \
	type *acursor = (type *)(a->dataptr + (base_stride * i));                                \
	type *bcursor = (type *)(b->dataptr + (base_stride * i));                                \
	for(int j = 0; j < sub_len; j++)                                                         \
	{                                                                                        \
	    result[j] = acursor[j] * bcursor[j];                                                 \
	}                                                                                        \
    }                                                                                            \
                                                                                                 \
    return c;                                                                                    \
}

/* sample expansion
NDArray *ndarray_mul_double_mp(NDArray *a, NDArray *b)
{
    // allocate result array
    NDArray *c = ndarray_new(a->ndim, a->dims, a->elem_bytes, NULL);
    if(!c)
    {
	return NULL;
    }

    intptr_t elem_stride = a->elem_bytes;
    intptr_t base_stride = (a->num_elems * elem_stride) / a->dims[0]; // stride of first dimension
    intptr_t sub_len = base_stride / elem_stride; // number of elements within each iteration of 1st dimension

    #pragma omp parallel for
    for(int i = 0; i < a->dims[0]; i++)
    {
	double *result = (double *)(c->dataptr + (base_stride * i));
	double *acursor = (double *)(a->dataptr + (base_stride * i));
	double *bcursor = (double *)(b->dataptr + (base_stride * i));
	for(int j = 0; j < sub_len; j++)
	{
	    result[j] = acursor[j] * bcursor[j];
	}
    }
    
    return c;
}
*/

MAKE_NDARRAY_OP_MP_FUNC(mul, *, float)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, double)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, int8_t)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, int16_t)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, int32_t)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, int64_t)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, uint8_t)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, uint16_t)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, uint32_t)
MAKE_NDARRAY_OP_MP_FUNC(mul, *, uint64_t)

MAKE_NDARRAY_OP_MP_FUNC(add, +, float)
MAKE_NDARRAY_OP_MP_FUNC(add, +, double)
MAKE_NDARRAY_OP_MP_FUNC(add, +, int8_t)
MAKE_NDARRAY_OP_MP_FUNC(add, +, int16_t)
MAKE_NDARRAY_OP_MP_FUNC(add, +, int32_t)
MAKE_NDARRAY_OP_MP_FUNC(add, +, int64_t)
MAKE_NDARRAY_OP_MP_FUNC(add, +, uint8_t)
MAKE_NDARRAY_OP_MP_FUNC(add, +, uint16_t)
MAKE_NDARRAY_OP_MP_FUNC(add, +, uint32_t)
MAKE_NDARRAY_OP_MP_FUNC(add, +, uint64_t)

MAKE_NDARRAY_OP_MP_FUNC(sub, -, float)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, double)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, int8_t)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, int16_t)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, int32_t)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, int64_t)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, uint8_t)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, uint16_t)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, uint32_t)
MAKE_NDARRAY_OP_MP_FUNC(sub, -, uint64_t)

MAKE_NDARRAY_OP_MP_FUNC(div, /, float)
MAKE_NDARRAY_OP_MP_FUNC(div, /, double)
MAKE_NDARRAY_OP_MP_FUNC(div, /, int8_t)
MAKE_NDARRAY_OP_MP_FUNC(div, /, int16_t)
MAKE_NDARRAY_OP_MP_FUNC(div, /, int32_t)
MAKE_NDARRAY_OP_MP_FUNC(div, /, int64_t)
MAKE_NDARRAY_OP_MP_FUNC(div, /, uint8_t)
MAKE_NDARRAY_OP_MP_FUNC(div, /, uint16_t)
MAKE_NDARRAY_OP_MP_FUNC(div, /, uint32_t)
MAKE_NDARRAY_OP_MP_FUNC(div, /, uint64_t)

MAKE_NDARRAY_OP_FUNC(mul, *, float)
MAKE_NDARRAY_OP_FUNC(mul, *, double)
MAKE_NDARRAY_OP_FUNC(mul, *, int8_t)
MAKE_NDARRAY_OP_FUNC(mul, *, int16_t)
MAKE_NDARRAY_OP_FUNC(mul, *, int32_t)
MAKE_NDARRAY_OP_FUNC(mul, *, int64_t)
MAKE_NDARRAY_OP_FUNC(mul, *, uint8_t)
MAKE_NDARRAY_OP_FUNC(mul, *, uint16_t)
MAKE_NDARRAY_OP_FUNC(mul, *, uint32_t)
MAKE_NDARRAY_OP_FUNC(mul, *, uint64_t)

MAKE_NDARRAY_OP_FUNC(add, +, float)
MAKE_NDARRAY_OP_FUNC(add, +, double)
MAKE_NDARRAY_OP_FUNC(add, +, int8_t)
MAKE_NDARRAY_OP_FUNC(add, +, int16_t)
MAKE_NDARRAY_OP_FUNC(add, +, int32_t)
MAKE_NDARRAY_OP_FUNC(add, +, int64_t)
MAKE_NDARRAY_OP_FUNC(add, +, uint8_t)
MAKE_NDARRAY_OP_FUNC(add, +, uint16_t)
MAKE_NDARRAY_OP_FUNC(add, +, uint32_t)
MAKE_NDARRAY_OP_FUNC(add, +, uint64_t)

MAKE_NDARRAY_OP_FUNC(sub, -, float)
MAKE_NDARRAY_OP_FUNC(sub, -, double)
MAKE_NDARRAY_OP_FUNC(sub, -, int8_t)
MAKE_NDARRAY_OP_FUNC(sub, -, int16_t)
MAKE_NDARRAY_OP_FUNC(sub, -, int32_t)
MAKE_NDARRAY_OP_FUNC(sub, -, int64_t)
MAKE_NDARRAY_OP_FUNC(sub, -, uint8_t)
MAKE_NDARRAY_OP_FUNC(sub, -, uint16_t)
MAKE_NDARRAY_OP_FUNC(sub, -, uint32_t)
MAKE_NDARRAY_OP_FUNC(sub, -, uint64_t)

MAKE_NDARRAY_OP_FUNC(div, /, float)
MAKE_NDARRAY_OP_FUNC(div, /, double)
MAKE_NDARRAY_OP_FUNC(div, /, int8_t)
MAKE_NDARRAY_OP_FUNC(div, /, int16_t)
MAKE_NDARRAY_OP_FUNC(div, /, int32_t)
MAKE_NDARRAY_OP_FUNC(div, /, int64_t)
MAKE_NDARRAY_OP_FUNC(div, /, uint8_t)
MAKE_NDARRAY_OP_FUNC(div, /, uint16_t)
MAKE_NDARRAY_OP_FUNC(div, /, uint32_t)
MAKE_NDARRAY_OP_FUNC(div, /, uint64_t)
    
/* 
   allocates an NDArray to store the result and returns a pointer to it
*/
#define MAKE_NDARRAY_ITER_OP_FUNC(name, op, type)	                                         \
NDArray *ndarray_iter_##name##_##type(NDArrayIter *a, NDArrayIter *b)                            \
{                                                                                                \
    NDArrayMultiIter *mit = ndarray_multi_iter_new_from_iter(2, a, b);                           \
                                                                                                 \
    if(!mit)                                                                                     \
    {                                                                                            \
	return NULL;                                                                             \
    }                                                                                            \
                                                                                                 \
    /* todo: add assert to check A and B elem_bytes are equal to sizeof(type) */                 \
                                                                                                 \
    /* allocate result array */                                                                  \
    NDArray *c = ndarray_new_result(mit, sizeof(type));                                          \
    if(!c)                                                                                       \
    {                                                                                            \
	ndarray_multi_iter_free_except_iter(mit);                                                \
	return NULL;                                                                             \
    }                                                                                            \
                                                                                                 \
    type *result = (type *) NDARRAY_DATAPTR(c);                                                  \
    do                                                                                           \
    {                                                                                            \
	*result++ = MULTI_ITER_DATA(mit, 0, type) op MULTI_ITER_DATA(mit, 1, type);              \
    } while(ndarray_multi_iter_next(mit));                                                       \
                                                                                                 \
    /* don't free the iterators that were passed in */                                           \
    ndarray_multi_iter_free_except_iter(mit);                  					 \
                                                                                                 \
    return c;                                                                                    \
}


/* sample expansion
NDArray *ndarray_iter_mul_double(NDArrayIter *a, NDArrayIter *b)
{
    NDArrayMultiIter *mit = ndarray_multi_iter_new_from_iter(2, a, b);
    
    if(!mit)
    {
	return NULL;
    }

    // todo: add assert to check A and B elem_bytes are equal to sizeof(double)
    
    // allocate result array
    NDArray *c = ndarray_new_result(mit, sizeof(double));
    if(!c)
    {
	ndarray_multi_iter_free_except_iter(mit);
	return NULL;
    }

    double *result = (double *) NDARRAY_DATAPTR(c);
    do
    {
	*result++ = MULTI_ITER_DATA(mit, 0, double) * MULTI_ITER_DATA(mit, 1, double);
    } while(ndarray_multi_iter_next(mit));

    // don't free the iterators that were passed in
    ndarray_multi_iter_free_except_iter(mit);
    
    return c;
}
*/

MAKE_NDARRAY_ITER_OP_FUNC(mul, *, float)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, double)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, int8_t)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, int16_t)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, int32_t)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, int64_t)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, uint8_t)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, uint16_t)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, uint32_t)
MAKE_NDARRAY_ITER_OP_FUNC(mul, *, uint64_t)

MAKE_NDARRAY_ITER_OP_FUNC(add, +, float)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, double)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, int8_t)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, int16_t)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, int32_t)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, int64_t)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, uint8_t)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, uint16_t)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, uint32_t)
MAKE_NDARRAY_ITER_OP_FUNC(add, +, uint64_t)

MAKE_NDARRAY_ITER_OP_FUNC(sub, -, float)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, double)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, int8_t)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, int16_t)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, int32_t)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, int64_t)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, uint8_t)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, uint16_t)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, uint32_t)
MAKE_NDARRAY_ITER_OP_FUNC(sub, -, uint64_t)

MAKE_NDARRAY_ITER_OP_FUNC(div, /, float)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, double)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, int8_t)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, int16_t)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, int32_t)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, int64_t)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, uint8_t)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, uint16_t)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, uint32_t)
MAKE_NDARRAY_ITER_OP_FUNC(div, /, uint64_t)

/* 
   allocates an NDArray to store the result and returns a pointer to it
*/    
#define MAKE_NDARRAY_FUN1_FUNC(name, fun, type)                            \
NDArray *ndarray_##name##_##type(NDArray *a, type y)                       \
{                                                                          \
    /* allocate result array */                                            \
    NDArray *c = ndarray_new(a->ndim, a->dims, sizeof(type), NULL);        \
    if(!c)                                                                 \
    {                                                                      \
	return NULL;                                                       \
    }                                                                      \
                                                                           \
    type *data = (type *)NDARRAY_DATAPTR(a);                               \
    type *result = (type *)NDARRAY_DATAPTR(c);                             \
                                                                           \
    if(a->num_elems > OPENMP_ELEM_THRESHOLD)                               \
    {                                                                      \
        _Pragma("omp parallel for shared(data)")                           \
	for(int i = 0; i < a->num_elems; i++)                              \
	{                                                                  \
	    result[i] = fun(data[i], y);                                   \
	}                                                                  \
    }                                                                      \
    else                                                                   \
    {	                                                                   \
	for(int i = 0; i < a->num_elems; i++)                              \
	{                                                                  \
	    result[i] = fun(data[i], y);                                   \
	}                                                                  \
    }                                                                      \
                                                                           \
    return c;                                                              \
}


/* sample expansion
NDArray *ndarray_pow_double(NDArray *a, double y)
{
    // allocate result array
    NDArray *c = ndarray_new(a->ndim, a->dims, sizeof(double), NULL);
    if(!c)
    {
	return NULL;
    }
    
    double *data = (double *)NDARRAY_DATAPTR(a);
    double *result = (double *)NDARRAY_DATAPTR(c);

    if(a->num_elems > OPENMP_ELEM_THRESHOLD)
    {
        #pragma omp parallel for shared(data)
	for(int i = 0; i < a->num_elems; i++)
	{
	    result[i] = pow(data[i], y);
	}
    }
    else
    {	
	for(int i = 0; i < a->num_elems; i++)
	{
	    result[i] = pow(data[i], y);
	}
    }
    
    return c;
}
*/

MAKE_NDARRAY_FUN1_FUNC(expt, powf, float)
MAKE_NDARRAY_FUN1_FUNC(expt, pow, double)

/* 
   allocates an NDArray to store the result and returns a pointer to it
*/    
#define MAKE_NDARRAY_ITER_FUN1_FUNC(name, fun, type)                       \
NDArray *ndarray_iter_##name##_##type(NDArrayIter *a, type y)              \
{                                                                          \
    /* allocate result array */                                            \
    NDArray *c = ndarray_new_result_single(a, sizeof(type));               \
    if(!c)                                                                 \
    {                                                                      \
	return NULL;                                                       \
    }                                                                      \
                                                                           \
    type *result = (type *) NDARRAY_DATAPTR(c);                            \
    do                                                                     \
    {                                                                      \
	*result++ = fun(ITER_DATA(a, type), y);                            \
    } while(ndarray_iter_next(a));                                         \
                                                                           \
    return c;                                                              \
}

MAKE_NDARRAY_ITER_FUN1_FUNC(expt, powf, float)
MAKE_NDARRAY_ITER_FUN1_FUNC(expt, pow, double)
