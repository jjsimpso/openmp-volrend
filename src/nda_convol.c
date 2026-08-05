#include <stdint.h>
#include <stdbool.h>
#include <malloc.h>
#include <stdio.h>
#include <math.h>
#include <omp.h>

#include "ndarray.h"
#include "nda_types.h"


/* 
   the kernel must only include positive values 
*/
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

uint8_t *ndarray_convolve2d_vec3_uint8_t(NDArray *base, double *kernel, int kw, int kh, intptr_t x, intptr_t y, uint8_t *retval)
{
    intptr_t w = base->dims[1];
    uint8_t (*data)[w][3];
    data = (uint8_t (*)[w][3])NDARRAY_DATAPTR(base);

    double (*k)[kw] = (double (*)[kw])kernel;
    double val[3] = { 0.0 };
    int jrange = kh/2;
    int irange = kw/2;
    
    for(int j = -jrange; j <= jrange; j++)
    {
	for(int i = -irange; i <= irange; i++)
	{
	    val[0] += (double)data[y+j][x+i][0] * k[j+jrange][i+irange];
	    val[1] += (double)data[y+j][x+i][1] * k[j+jrange][i+irange];
	    val[2] += (double)data[y+j][x+i][2] * k[j+jrange][i+irange];
	}
    }

    retval[0] = (val[0] < UINT8_MAX) ? (uint8_t)val[0] : UINT8_MAX;
    retval[1] = (val[1] < UINT8_MAX) ? (uint8_t)val[1] : UINT8_MAX;
    retval[2] = (val[2] < UINT8_MAX) ? (uint8_t)val[2] : UINT8_MAX;
    
    return retval;
}
