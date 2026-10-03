#include "ndarray.h"
#include "nda_types.h"

typedef enum {
    NDARRAY_CONVOLVE_SKIP,
    NDARRAY_CONVOLVE_CLAMP,
} NDArrayConvolveMode;

uint8_t ndarray_convolve2d_point_uint8_t(NDArray *base, double *kernel, int kw, int kh, intptr_t x, intptr_t y);
uint16_t ndarray_convolve2d_point_uint16_t(NDArray *base, double *kernel, int kw, int kh, intptr_t x, intptr_t y);

NDArray *ndarray_convolve2d_uint8_t(NDArray *base, double *kernel, int kw, int kh, NDArrayConvolveMode mode);
NDArray *ndarray_convolve2d_uint16_t(NDArray *base, double *kernel, int kw, int kh, NDArrayConvolveMode mode);

NDArray *ndarray_convolve2d_vec3_uint8_t(NDArray *base, double *kernel, int kw, int kh, NDArrayConvolveMode mode);
NDArray *ndarray_convolve2d_vec3_uint16_t(NDArray *base, double *kernel, int kw, int kh, NDArrayConvolveMode mode);
