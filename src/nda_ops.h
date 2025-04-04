#include "ndarray.h"

/* function API */
void ndarray_fill_float(NDArray *a, float val);
void ndarray_fill_double(NDArray *a, double val);
void ndarray_fill_int8_t(NDArray *a, int8_t val);
void ndarray_fill_int16_t(NDArray *a, int16_t val);
void ndarray_fill_int32_t(NDArray *a, int32_t val);
void ndarray_fill_int64_t(NDArray *a, int64_t val);
void ndarray_fill_uint8_t(NDArray *a, uint8_t val);
void ndarray_fill_uint16_t(NDArray *a, uint16_t val);
void ndarray_fill_uint32_t(NDArray *a, uint32_t val);
void ndarray_fill_uint64_t(NDArray *a, uint64_t val);

void ndarray_fill_index_float(NDArray *a);
void ndarray_fill_index_double(NDArray *a);
void ndarray_fill_index_int8_t(NDArray *a);
void ndarray_fill_index_int16_t(NDArray *a);
void ndarray_fill_index_int32_t(NDArray *a);
void ndarray_fill_index_int64_t(NDArray *a);
void ndarray_fill_index_uint8_t(NDArray *a);
void ndarray_fill_index_uint16_t(NDArray *a);
void ndarray_fill_index_uint32_t(NDArray *a);
void ndarray_fill_index_uint64_t(NDArray *a);

float ndarray_sum_float(NDArray *a);
double ndarray_sum_double(NDArray *a);
int32_t ndarray_sum_int32_t(NDArray *a);
int64_t ndarray_sum_int64_t(NDArray *a);
uint32_t ndarray_sum_uint32_t(NDArray *a);
uint64_t ndarray_sum_uint64_t(NDArray *a);

NDArray *ndarray_mul_float_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_double_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_int8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_int16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_int32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_int64_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_uint8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_uint16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_uint32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_uint64_t_mp(NDArray *a, NDArray *b);

NDArray *ndarray_add_float_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_double_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_int8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_int16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_int32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_int64_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint64_t_mp(NDArray *a, NDArray *b);

NDArray *ndarray_mul_float(NDArray *a, NDArray *b);
NDArray *ndarray_mul_double(NDArray *a, NDArray *b);
NDArray *ndarray_mul_int8_t(NDArray *a, NDArray *b);
NDArray *ndarray_mul_int16_t(NDArray *a, NDArray *b);
NDArray *ndarray_mul_int32_t(NDArray *a, NDArray *b);
NDArray *ndarray_mul_int64_t(NDArray *a, NDArray *b);
NDArray *ndarray_mul_uint8_t(NDArray *a, NDArray *b);
NDArray *ndarray_mul_uint16_t(NDArray *a, NDArray *b);
NDArray *ndarray_mul_uint32_t(NDArray *a, NDArray *b);
NDArray *ndarray_mul_uint64_t(NDArray *a, NDArray *b);

NDArray *ndarray_add_float(NDArray *a, NDArray *b);
NDArray *ndarray_add_double(NDArray *a, NDArray *b);
NDArray *ndarray_add_int8_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_int16_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_int32_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_int64_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint8_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint16_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint32_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint64_t(NDArray *a, NDArray *b);

NDArray *ndarray_iter_mul_float(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_double(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_int8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_int16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_int32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_int64_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_uint8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_uint16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_uint32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_uint64_t(NDArrayIter *a, NDArrayIter *b);

NDArray *ndarray_iter_add_float(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_double(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_int8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_int16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_int32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_int64_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_uint8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_uint16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_uint32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_uint64_t(NDArrayIter *a, NDArrayIter *b);
