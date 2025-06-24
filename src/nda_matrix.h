#include "ndarray.h"

bool *ndarray_fill_mat_ident_float(NDArray *a);
bool *ndarray_fill_mat_ident_double(NDArray *a);
bool *ndarray_fill_mat_ident_int8_t(NDArray *a);
bool *ndarray_fill_mat_ident_int16_t(NDArray *a);
bool *ndarray_fill_mat_ident_int32_t(NDArray *a);
bool *ndarray_fill_mat_ident_int64_t(NDArray *a);
bool *ndarray_fill_mat_ident_uint8_t(NDArray *a);
bool *ndarray_fill_mat_ident_uint16_t(NDArray *a);
bool *ndarray_fill_mat_ident_uint32_t(NDArray *a);
bool *ndarray_fill_mat_ident_uint64_t(NDArray *a);

NDArray *ndarray_matmul_float_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_double_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_int8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_int16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_int32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_int64_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_uint8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_uint16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_uint32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_uint64_t_mp(NDArray *a, NDArray *b);

NDArray *ndarray_matmul_float(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_double(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_int8_t(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_int16_t(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_int32_t(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_int64_t(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_uint8_t(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_uint16_t(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_uint32_t(NDArray *a, NDArray *b);
NDArray *ndarray_matmul_uint64_t(NDArray *a, NDArray *b);

NDArray *ndarray_iter_matmul_float(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_double(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_int8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_int16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_int32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_int64_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_uint8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_uint16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_uint32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_matmul_uint64_t(NDArrayIter *a, NDArrayIter *b);
