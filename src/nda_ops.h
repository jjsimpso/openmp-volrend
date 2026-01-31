#include <complex.h>
#include "ndarray.h"

/* function API */
void ndarray_fill_float(NDArray *a, float val);
void ndarray_fill_double(NDArray *a, double val);
void ndarray_fill_complex(NDArray *a, complex val);
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
complex ndarray_sum_complex(NDArray *a);
int32_t ndarray_sum_int32_t(NDArray *a);
int64_t ndarray_sum_int64_t(NDArray *a);
uint32_t ndarray_sum_uint32_t(NDArray *a);
uint64_t ndarray_sum_uint64_t(NDArray *a);

float ndarray_iter_sum_float(NDArrayIter *a);
double ndarray_iter_sum_double(NDArrayIter *a);
complex ndarray_iter_sum_complex(NDArrayIter *a);
int32_t ndarray_iter_sum_int32_t(NDArrayIter *a);
int64_t ndarray_iter_sum_int64_t(NDArrayIter *a);
uint32_t ndarray_iter_sum_uint32_t(NDArrayIter *a);
uint64_t ndarray_iter_sum_uint64_t(NDArrayIter *a);

bool ndarray_equal(NDArray *a, NDArray *b);

bool ndarray_iter_equal_float(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_double(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_int8_t(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_int16_t(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_int32_t(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_int64_t(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_uint8_t(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_uint16_t(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_uint32_t(NDArrayIter *a, NDArrayIter *b);
bool ndarray_iter_equal_uint64_t(NDArrayIter *a, NDArrayIter *b);

NDArray *ndarray_mul_float_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_double_mp(NDArray *a, NDArray *b);
NDArray *ndarray_mul_complex_mp(NDArray *a, NDArray *b);
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
NDArray *ndarray_add_complex_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_int8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_int16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_int32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_int64_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint64_t_mp(NDArray *a, NDArray *b);

NDArray *ndarray_sub_float_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_double_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_complex_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_int8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_int16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_int32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_int64_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_uint8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_uint16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_uint32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_sub_uint64_t_mp(NDArray *a, NDArray *b);

NDArray *ndarray_div_float_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_double_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_complex_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_int8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_int16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_int32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_int64_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_uint8_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_uint16_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_uint32_t_mp(NDArray *a, NDArray *b);
NDArray *ndarray_div_uint64_t_mp(NDArray *a, NDArray *b);

NDArray *ndarray_mul_float(NDArray *a, NDArray *b);
NDArray *ndarray_mul_double(NDArray *a, NDArray *b);
NDArray *ndarray_mul_complex(NDArray *a, NDArray *b);
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
NDArray *ndarray_add_complex(NDArray *a, NDArray *b);
NDArray *ndarray_add_int8_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_int16_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_int32_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_int64_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint8_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint16_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint32_t(NDArray *a, NDArray *b);
NDArray *ndarray_add_uint64_t(NDArray *a, NDArray *b);

NDArray *ndarray_sub_float(NDArray *a, NDArray *b);
NDArray *ndarray_sub_double(NDArray *a, NDArray *b);
NDArray *ndarray_sub_complex(NDArray *a, NDArray *b);
NDArray *ndarray_sub_int8_t(NDArray *a, NDArray *b);
NDArray *ndarray_sub_int16_t(NDArray *a, NDArray *b);
NDArray *ndarray_sub_int32_t(NDArray *a, NDArray *b);
NDArray *ndarray_sub_int64_t(NDArray *a, NDArray *b);
NDArray *ndarray_sub_uint8_t(NDArray *a, NDArray *b);
NDArray *ndarray_sub_uint16_t(NDArray *a, NDArray *b);
NDArray *ndarray_sub_uint32_t(NDArray *a, NDArray *b);
NDArray *ndarray_sub_uint64_t(NDArray *a, NDArray *b);

NDArray *ndarray_div_float(NDArray *a, NDArray *b);
NDArray *ndarray_div_double(NDArray *a, NDArray *b);
NDArray *ndarray_div_complex(NDArray *a, NDArray *b);
NDArray *ndarray_div_int8_t(NDArray *a, NDArray *b);
NDArray *ndarray_div_int16_t(NDArray *a, NDArray *b);
NDArray *ndarray_div_int32_t(NDArray *a, NDArray *b);
NDArray *ndarray_div_int64_t(NDArray *a, NDArray *b);
NDArray *ndarray_div_uint8_t(NDArray *a, NDArray *b);
NDArray *ndarray_div_uint16_t(NDArray *a, NDArray *b);
NDArray *ndarray_div_uint32_t(NDArray *a, NDArray *b);
NDArray *ndarray_div_uint64_t(NDArray *a, NDArray *b);

NDArray *ndarray_iter_mul_float(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_double(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_mul_complex(NDArrayIter *a, NDArrayIter *b);
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
NDArray *ndarray_iter_add_complex(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_int8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_int16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_int32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_int64_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_uint8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_uint16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_uint32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_add_uint64_t(NDArrayIter *a, NDArrayIter *b);

NDArray *ndarray_iter_sub_float(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_double(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_complex(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_int8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_int16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_int32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_int64_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_uint8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_uint16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_uint32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_sub_uint64_t(NDArrayIter *a, NDArrayIter *b);

NDArray *ndarray_iter_div_float(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_double(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_complex(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_int8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_int16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_int32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_int64_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_uint8_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_uint16_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_uint32_t(NDArrayIter *a, NDArrayIter *b);
NDArray *ndarray_iter_div_uint64_t(NDArrayIter *a, NDArrayIter *b);

NDArray *ndarray_sqrt_float(NDArray *a);
NDArray *ndarray_sqrt_double(NDArray *a);

NDArray *ndarray_iter_sqrt_float(NDArrayIter *a);
NDArray *ndarray_iter_sqrt_double(NDArrayIter *a);

NDArray *ndarray_expt_float(NDArray *a, float y);
NDArray *ndarray_expt_double(NDArray *a, double y);

NDArray *ndarray_iter_expt_float(NDArrayIter *a, float y);
NDArray *ndarray_iter_expt_double(NDArrayIter *a, double y);
