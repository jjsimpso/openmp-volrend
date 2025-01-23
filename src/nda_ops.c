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
   allocates an NDArray to store the result and returns a pointer to it
*/
#define MAKE_NDARRAY_OP_FUNC(name, op, type)	                                                 \
NDArray *ndarray_##name##_##type(NDArray *a, NDArray *b)                                         \
{                                                                                                \
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
NDArray *ndarray_iter_mul(NDArrayIter *a, NDArrayIter *b)
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
