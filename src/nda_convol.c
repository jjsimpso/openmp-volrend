#include <stdint.h>
#include <stdbool.h>
#include <malloc.h>
#include <stdio.h>
#include <math.h>
#include <omp.h>

#include "ndarray.h"

uint8_t ndarray_convolve2d_uint8_t(NDArray *base, double *kernel, int kw, int kh, intptr_t x, intptr_t y)
{
    intptr_t w = base->dims[1];
    uint8_t (*data)[w];
    data = (uint8_t (*)[w])NDARRAY_DATAPTR(base);

    double (*k)[kw] = (double (*)[kw])kernel;
    double val = 0.0;
    int jrange = kh/2;
    int irange = kw/2;
    
    for(int j = -jrange; j <= jrange; j++)
    {
	for(int i = -irange; i <= irange; i++)
	{
	    val += (double)data[y+j][x+i] * k[j+jrange][i+irange];
	}
    }

    return (val < UINT8_MAX) ? (uint8_t)val : UINT8_MAX;
}

