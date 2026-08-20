#include <stdint.h>
#include <stdbool.h>
#include <malloc.h>
#include <stdio.h>
#include <math.h>
#include <omp.h>

#include "ndarray.h"
#include "nda_types.h"


/* 

*/
uint8_t ndarray_convolve2d_point_uint8_t(NDArray *base, double *kernel, int kw, int kh, intptr_t x, intptr_t y)
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

    return (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
}

uint8_t *ndarray_convolve2d_point_vec3_uint8_t(NDArray *base, double *kernel, int kw, int kh, intptr_t x, intptr_t y, uint8_t *retval)
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

    retval[0] = (val[0] < UINT8_MAX) ? (uint8_t)((val[0] < 0.0) ? 0 : val[0]) : UINT8_MAX;
    retval[1] = (val[1] < UINT8_MAX) ? (uint8_t)((val[1] < 0.0) ? 0 : val[1]) : UINT8_MAX;
    retval[2] = (val[2] < UINT8_MAX) ? (uint8_t)((val[2] < 0.0) ? 0 : val[2]) : UINT8_MAX;
    
    return retval;
}

NDArray *ndarray_convolve2d_uint8_t(NDArray *base, double *kernel, int kw, int kh)
{
    intptr_t h = base->dims[0];
    intptr_t w = base->dims[1];
    uint8_t (*in_data)[w];
    in_data = (uint8_t (*)[w])NDARRAY_DATAPTR(base);

    NDArray *out = ndarray_new(2, (intptr_t []){h, w}, sizeof(uint8_t), NULL);
    if(!out)
    {
	return NULL;
    }
    
    uint8_t (*out_data)[w];
    out_data = (uint8_t (*)[w])NDARRAY_DATAPTR(out);
    
    double (*k)[kw] = (double (*)[kw])kernel;
    int jrange = kh/2;
    int irange = kw/2;

    /* handle literal edge cases where kernel extends outside of data array by clamping values */
    /* top */
    for(int y = 0; y < jrange; y++)
    {
	for(int x = irange; x < (w - irange); x++)
	{
	    double val = 0.0;
	    for(int j = -jrange; j <= jrange; j++)
	    {
		int yidx = ((y+j) < 0) ? 0 : (y+j);
		for(int i = -irange; i <= irange; i++)
		{
		    val += (double)in_data[yidx][x+i] * k[j+jrange][i+irange];
		}
	    }
	    out_data[y][x] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
	}
    }

    /* bottom */
    for(int y = (h - jrange); y < h; y++)
    {
	for(int x = irange; x < (w - irange); x++)
	{
	    double val = 0.0;
	    for(int j = -jrange; j <= jrange; j++)
	    {
		int yidx = ((y+j) > (h-1)) ? (h-1) : (y+j);
		for(int i = -irange; i <= irange; i++)
		{
		    val += (double)in_data[yidx][x+i] * k[j+jrange][i+irange];
		}
	    }
	    out_data[y][x] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
	}
    }

    /* left */
    for(int y = jrange; y < (h - jrange); y++)
    {
	for(int x = 0; x < irange; x++)
	{
	    double val = 0.0;
	    for(int j = -jrange; j <= jrange; j++)
	    {
		for(int i = -irange; i <= irange; i++)
		{
		    int xidx = ((x+i) < 0) ? 0 : (x+i);
		    val += (double)in_data[y+j][xidx] * k[j+jrange][i+irange];
		}
	    }
	    out_data[y][x] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
	}
    }

    /* right */
    for(int y = jrange; y < (h - jrange); y++)
    {
	for(int x = (w - irange); x < w; x++)
	{
	    double val = 0.0;
	    for(int j = -jrange; j <= jrange; j++)
	    {
		for(int i = -irange; i <= irange; i++)
		{
		    int xidx = ((x+i) > (w-1)) ? (w-1) : (x+i);
		    val += (double)in_data[y+j][xidx] * k[j+jrange][i+irange];
		}
	    }
	    out_data[y][x] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
	}

    }
    
    /* the rest */
    for(int y = jrange; y < (h - jrange); y++)
    {
	for(int x = irange; x < (w - irange); x++)
	{
	    double val = 0.0;
	    for(int j = -jrange; j <= jrange; j++)
	    {
		for(int i = -irange; i <= irange; i++)
		{
		    val += (double)in_data[y+j][x+i] * k[j+jrange][i+irange];
		}
	    }
	    out_data[y][x] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
	}
    }

    return out;
}
