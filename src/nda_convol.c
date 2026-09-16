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

#define MAKE_NDARRAY_CONVOLVE2D_FUNC(type, maxval)                                                       \
NDArray *ndarray_convolve2d_##type(NDArray *base, double *kernel, int kw, int kh)                        \
{                                                                                                        \
    intptr_t h = base->dims[0];                                                                          \
    intptr_t w = base->dims[1];                                                                          \
    type (*in_data)[w];                                                                                  \
    in_data = (type (*)[w])NDARRAY_DATAPTR(base);                                                        \
                                                                                                         \
    NDArray *out = ndarray_new(2, (intptr_t []){h, w}, sizeof(type), NULL);                              \
    if(!out)                                                                                             \
    {                                                                                                    \
        return NULL;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    type (*out_data)[w];                                                                                 \
    out_data = (type (*)[w])NDARRAY_DATAPTR(out);                                                        \
                                                                                                         \
    double (*k)[kw] = (double (*)[kw])kernel;                                                            \
    int jrange = kh/2;                                                                                   \
    int irange = kw/2;                                                                                   \
                                                                                                         \
    /* handle literal edge cases where kernel extends outside of data array by clamping values */        \
    /* top */                                                                                            \
    for(int y = 0; y < jrange; y++)                                                                      \
    {                                                                                                    \
        for(int x = irange; x < (w - irange); x++)                                                       \
        {                                                                                                \
            double val = 0.0;                                                                            \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                int yidx = ((y+j) < 0) ? 0 : (y+j);                                                      \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    val += (double)in_data[yidx][x+i] * k[j+jrange][i+irange];                           \
                }                                                                                        \
            }                                                                                            \
            out_data[y][x] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                    \
        }                                                                                                \
    }                                                                                                    \
                                                                                                         \
    /* bottom */                                                                                         \
    for(int y = (h - jrange); y < h; y++)                                                                \
    {                                                                                                    \
        for(int x = irange; x < (w - irange); x++)                                                       \
        {                                                                                                \
            double val = 0.0;                                                                            \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                int yidx = ((y+j) > (h-1)) ? (h-1) : (y+j);                                              \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    val += (double)in_data[yidx][x+i] * k[j+jrange][i+irange];                           \
                }                                                                                        \
            }                                                                                            \
            out_data[y][x] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                    \
        }                                                                                                \
    }                                                                                                    \
                                                                                                         \
    /* left */                                                                                           \
    for(int y = jrange; y < (h - jrange); y++)                                                           \
    {                                                                                                    \
        for(int x = 0; x < irange; x++)                                                                  \
        {                                                                                                \
            double val = 0.0;                                                                            \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    int xidx = ((x+i) < 0) ? 0 : (x+i);                                                  \
                    val += (double)in_data[y+j][xidx] * k[j+jrange][i+irange];                           \
                }                                                                                        \
            }                                                                                            \
            out_data[y][x] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                    \
        }                                                                                                \
    }                                                                                                    \
                                                                                                         \
    /* right */                                                                                          \
    for(int y = jrange; y < (h - jrange); y++)                                                           \
    {                                                                                                    \
        for(int x = (w - irange); x < w; x++)                                                            \
        {                                                                                                \
            double val = 0.0;                                                                            \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    int xidx = ((x+i) > (w-1)) ? (w-1) : (x+i);                                          \
                    val += (double)in_data[y+j][xidx] * k[j+jrange][i+irange];                           \
                }                                                                                        \
            }                                                                                            \
            out_data[y][x] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                    \
        }                                                                                                \
                                                                                                         \
    }                                                                                                    \
                                                                                                         \
    /* handle 4 corners */                                                                               \
    for(int y = 0; y < jrange; y++)                                                                      \
    {                                                                                                    \
        for(int x = 0; x < irange; x++)                                                                  \
        {                                                                                                \
            /* top left */                                                                               \
            /*printf("top left corner:\n");*/                                                            \
            double val = 0.0;                                                                            \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                int yidx = ((y+j) < 0) ? 0 : (y+j);                                                      \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    int xidx = ((x+i) < 0) ? 0 : (x+i);                                                  \
                    val += (double)in_data[yidx][xidx] * k[j+jrange][i+irange];                          \
                    /*printf(" %dx%d = %.2f\n", xidx, yidx, in_data[yidx][xidx] * k[j+jrange][i+irange]);*/ \
                }                                                                                        \
            }                                                                                            \
            out_data[y][x] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                    \
                                                                                                         \
            /* x,y coordinates for bottom right corner */                                                \
            int cx = (w - irange) + x;                                                                   \
            int cy = (h - jrange) + y;                                                                   \
                                                                                                         \
            /* top right */                                                                              \
            /*printf("top right corner:\n");*/                                                           \
            val = 0.0;                                                                                   \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                int yidx = ((y+j) < 0) ? 0 : (y+j);                                                      \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    int xidx = ((cx+i) > (w-1)) ? (w-1) : (cx+i);                                        \
                    val += (double)in_data[yidx][xidx] * k[j+jrange][i+irange];                          \
                    /*printf(" %dx%d\n", xidx, yidx);*/                                                  \
                }                                                                                        \
            }                                                                                            \
            out_data[y][cx] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                   \
                                                                                                         \
            /* bottom left */                                                                            \
            /*printf("bottom left corner:\n");*/                                                         \
            val = 0.0;                                                                                   \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                int yidx = ((cy+j) > (h-1)) ? (h-1) : (cy+j);                                            \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    int xidx = ((x+i) < 0) ? 0 : (x+i);                                                  \
                    val += (double)in_data[yidx][xidx] * k[j+jrange][i+irange];                          \
                    /*printf(" %dx%d\n", xidx, yidx);*/                                                  \
                }                                                                                        \
            }                                                                                            \
            out_data[cy][x] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                   \
                                                                                                         \
            /* bottom right */                                                                           \
            /*printf("bottom right corner:\n");*/                                                        \
            val = 0.0;                                                                                   \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                int yidx = ((cy+j) > (h-1)) ? (h-1) : (cy+j);                                            \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    int xidx = ((cx+i) > (w-1)) ? (w-1) : (cx+i);                                        \
                    val += (double)in_data[yidx][xidx] * k[j+jrange][i+irange];                          \
                    /*printf(" %dx%d\n", xidx, yidx);*/                                                  \
                }                                                                                        \
            }                                                                                            \
            out_data[cy][cx] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                  \
        }                                                                                                \
    }                                                                                                    \
                                                                                                         \
    /* the rest */                                                                                       \
    /* Attempt to use framework desktop gpu. Fails because host and device have unified memory.*/        \
    /* Unfortunately Fedora 44 GCC doesn't have support for unified memory on this gpu. Trying */        \
    /* to explicitly map leads to a conflict.                                                  */        \
    /*_Pragma("omp target map(tofrom: in_data[0:w*h], out_data[0:w*h], k[0:kw*kh])")           */        \
    /*_Pragma("omp loop")                                                                      */        \
    _Pragma("omp parallel for")                                                                          \
    for(int y = jrange; y < (h - jrange); y++)                                                           \
    {                                                                                                    \
        for(int x = irange; x < (w - irange); x++)                                                       \
        {                                                                                                \
            double val = 0.0;                                                                            \
            for(int j = -jrange; j <= jrange; j++)                                                       \
            {                                                                                            \
                for(int i = -irange; i <= irange; i++)                                                   \
                {                                                                                        \
                    val += (double)in_data[y+j][x+i] * k[j+jrange][i+irange];                            \
                }                                                                                        \
            }                                                                                            \
            out_data[y][x] = (val < maxval) ? (type)((val < 0.0) ? 0 : val) : maxval;                    \
        }                                                                                                \
    }                                                                                                    \
                                                                                                         \
    return out;                                                                                          \
}

MAKE_NDARRAY_CONVOLVE2D_FUNC(uint8_t, UINT8_MAX)
MAKE_NDARRAY_CONVOLVE2D_FUNC(uint16_t, UINT16_MAX)


#define MAKE_NDARRAY_CONVOLVE2D_VEC_FUNC(type, maxval, vlen)                                                \
NDArray *ndarray_convolve2d_vec##vlen##_##type(NDArray *base, double *kernel, int kw, int kh)               \
{                                                                                                           \
    intptr_t h = base->dims[0];                                                                             \
    intptr_t w = base->dims[1];                                                                             \
    type (*in_data)[w][vlen];                                                                               \
    in_data = (type (*)[w][vlen])NDARRAY_DATAPTR(base);                                                     \
                                                                                                            \
    NDArray *out = ndarray_new(3, (intptr_t []){h, w, vlen}, sizeof(type), NULL);                           \
    if(!out)                                                                                                \
    {                                                                                                       \
        return NULL;                                                                                        \
    }                                                                                                       \
                                                                                                            \
    type (*out_data)[w][vlen];                                                                              \
    out_data = (type (*)[w][vlen])NDARRAY_DATAPTR(out);                                                     \
                                                                                                            \
    double (*k)[kw] = (double (*)[kw])kernel;                                                               \
    int jrange = kh/2;                                                                                      \
    int irange = kw/2;                                                                                      \
                                                                                                            \
    /* handle literal edge cases where kernel extends outside of data array by clamping values */           \
    /* top */                                                                                               \
    for(int y = 0; y < jrange; y++)                                                                         \
    {                                                                                                       \
        for(int x = irange; x < (w - irange); x++)                                                          \
        {                                                                                                   \
            double val[vlen] = {0.0};                                                                       \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                int yidx = ((y+j) < 0) ? 0 : (y+j);                                                         \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[yidx][x+i][z] * k[j+jrange][i+irange];                    \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
        }                                                                                                   \
    }                                                                                                       \
                                                                                                            \
    /* bottom */                                                                                            \
    for(int y = (h - jrange); y < h; y++)                                                                   \
    {                                                                                                       \
        for(int x = irange; x < (w - irange); x++)                                                          \
        {                                                                                                   \
            double val[vlen] = {0.0};                                                                       \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                int yidx = ((y+j) > (h-1)) ? (h-1) : (y+j);                                                 \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[yidx][x+i][z] * k[j+jrange][i+irange];                    \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
        }                                                                                                   \
    }                                                                                                       \
                                                                                                            \
    /* left */                                                                                              \
    for(int y = jrange; y < (h - jrange); y++)                                                              \
    {                                                                                                       \
        for(int x = 0; x < irange; x++)                                                                     \
        {                                                                                                   \
            double val[vlen] = {0.0};                                                                       \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    int xidx = ((x+i) < 0) ? 0 : (x+i);                                                     \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[y+j][xidx][z] * k[j+jrange][i+irange];                    \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
        }                                                                                                   \
    }                                                                                                       \
                                                                                                            \
    /* right */                                                                                             \
    for(int y = jrange; y < (h - jrange); y++)                                                              \
    {                                                                                                       \
        for(int x = (w - irange); x < w; x++)                                                               \
        {                                                                                                   \
            double val[vlen] = {0.0};                                                                       \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    int xidx = ((x+i) > (w-1)) ? (w-1) : (x+i);                                             \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[y+j][xidx][z] * k[j+jrange][i+irange];                    \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
        }                                                                                                   \
                                                                                                            \
    }                                                                                                       \
                                                                                                            \
    /* handle 4 corners */                                                                                  \
    for(int y = 0; y < jrange; y++)                                                                         \
    {                                                                                                       \
        for(int x = 0; x < irange; x++)                                                                     \
        {                                                                                                   \
            /* top left */                                                                                  \
            /*printf("top left corner:\n");*/                                                               \
            double val[vlen] = {0.0};                                                                       \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                int yidx = ((y+j) < 0) ? 0 : (y+j);                                                         \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    int xidx = ((x+i) < 0) ? 0 : (x+i);                                                     \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[yidx][xidx][z] * k[j+jrange][i+irange];                   \
                        /*printf(" %dx%d = %.2f\n", xidx, yidx, in_data[yidx][xidx] * k[j+jrange][i+irange]);*/ \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
                                                                                                            \
            /* x,y coordinates for bottom right corner */                                                   \
            int cx = (w - irange) + x;                                                                      \
            int cy = (h - jrange) + y;                                                                      \
                                                                                                            \
            /* top right */                                                                                 \
            /*printf("top right corner:\n");*/                                                              \
            val[0] = val[1] = val[2] = 0.0;                                                                 \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                int yidx = ((y+j) < 0) ? 0 : (y+j);                                                         \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    int xidx = ((cx+i) > (w-1)) ? (w-1) : (cx+i);                                           \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[yidx][xidx][z] * k[j+jrange][i+irange];                   \
                        /*printf(" %dx%d\n", xidx, yidx);*/                                                 \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
                                                                                                            \
            /* bottom left */                                                                               \
            /*printf("bottom left corner:\n");*/                                                            \
            val[0] = val[1] = val[2] = 0.0;                                                                 \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                int yidx = ((cy+j) > (h-1)) ? (h-1) : (cy+j);                                               \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    int xidx = ((x+i) < 0) ? 0 : (x+i);                                                     \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[yidx][xidx][z] * k[j+jrange][i+irange];                   \
                        /*printf(" %dx%d\n", xidx, yidx);*/                                                 \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
                                                                                                            \
            /* bottom right */                                                                              \
            /*printf("bottom right corner:\n");*/                                                           \
            val[0] = val[1] = val[2] = 0.0;                                                                 \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                int yidx = ((cy+j) > (h-1)) ? (h-1) : (cy+j);                                               \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    int xidx = ((cx+i) > (w-1)) ? (w-1) : (cx+i);                                           \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[yidx][xidx][z] * k[j+jrange][i+irange];                   \
                        /*printf(" %dx%d\n", xidx, yidx);*/                                                 \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
        }                                                                                                   \
    }                                                                                                       \
                                                                                                            \
    /* the rest */                                                                                          \
    _Pragma("omp parallel for")                                                                             \
    for(int y = jrange; y < (h - jrange); y++)                                                              \
    {                                                                                                       \
        for(int x = irange; x < (w - irange); x++)                                                          \
        {                                                                                                   \
            double val[vlen] = {0.0};                                                                       \
            for(int j = -jrange; j <= jrange; j++)                                                          \
            {                                                                                               \
                for(int i = -irange; i <= irange; i++)                                                      \
                {                                                                                           \
                    for(int z = 0; z < vlen; z++)                                                           \
                    {                                                                                       \
                        val[z] += (double)in_data[y+j][x+i][z] * k[j+jrange][i+irange];                     \
                    }                                                                                       \
                }                                                                                           \
            }                                                                                               \
            for(int z = 0; z < vlen; z++)                                                                   \
            {                                                                                               \
                out_data[y][x][z] = (val[z] < maxval) ? (type)((val[z] < 0.0) ? 0 : val[z]) : maxval;       \
            }                                                                                               \
        }                                                                                                   \
    }                                                                                                       \
                                                                                                            \
    return out;                                                                                             \
}

MAKE_NDARRAY_CONVOLVE2D_VEC_FUNC(uint8_t, UINT8_MAX, 3)
MAKE_NDARRAY_CONVOLVE2D_VEC_FUNC(uint8_t, UINT8_MAX, 4)
MAKE_NDARRAY_CONVOLVE2D_VEC_FUNC(uint16_t, UINT16_MAX, 3)
MAKE_NDARRAY_CONVOLVE2D_VEC_FUNC(uint16_t, UINT16_MAX, 4)


/* 
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

    // handle literal edge cases where kernel extends outside of data array by clamping values
    // top
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

    // bottom
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

    // left
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

    // right
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

    // handle 4 corners
    for(int y = 0; y < jrange; y++)
    {
        for(int x = 0; x < irange; x++)
        {
            // top left
            printf("top left corner:\n");
            double val = 0.0;
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((y+j) < 0) ? 0 : (y+j);
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((x+i) < 0) ? 0 : (x+i);
                    val += (double)in_data[yidx][xidx] * k[j+jrange][i+irange];
                    printf(" %dx%d = %.2f\n", xidx, yidx, in_data[yidx][xidx] * k[j+jrange][i+irange]);
                }
            }
            out_data[y][x] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;

            // x,y coordinates for bottom right corner
            int cx = (w - irange) + x;
            int cy = (h - jrange) + y;
            
            // top right
            //printf("top right corner:\n");
            val = 0.0;
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((y+j) < 0) ? 0 : (y+j);
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((cx+i) > (w-1)) ? (w-1) : (cx+i);
                    val += (double)in_data[yidx][xidx] * k[j+jrange][i+irange];
                    //printf(" %dx%d\n", xidx, yidx);
                }
            }
            out_data[y][cx] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
            
            // bottom left
            //printf("bottom left corner:\n");
            val = 0.0;
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((cy+j) > (h-1)) ? (h-1) : (cy+j);
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((x+i) < 0) ? 0 : (x+i);
                    val += (double)in_data[yidx][xidx] * k[j+jrange][i+irange];
                    //printf(" %dx%d\n", xidx, yidx);
                }
            }
            out_data[cy][x] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
            
            // bottom right
            //printf("bottom right corner:\n");
            val = 0.0;
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((cy+j) > (h-1)) ? (h-1) : (cy+j);
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((cx+i) > (w-1)) ? (w-1) : (cx+i);
                    val += (double)in_data[yidx][xidx] * k[j+jrange][i+irange];
                    //printf(" %dx%d\n", xidx, yidx);
                }
            }
            out_data[cy][cx] = (val < UINT8_MAX) ? (uint8_t)((val < 0.0) ? 0 : val) : UINT8_MAX;
        }
    }
    
    // the rest
    _Pragma("omp parallel for")
    // Attempt to use framework desktop gpu. Fails because host and device have unified memory.
    // Unfortunately Fedora 44 GCC doesn't have support for unified memory on this gpu. Trying
    // to explicitly map leads to a conflict.
    //_Pragma("omp target map(tofrom: in_data[0:w*h], out_data[0:w*h], k[0:kw*kh])")
    //_Pragma("omp loop")
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
*/

/*
NDArray *ndarray_convolve2d_vec3_uint8_t(NDArray *base, double *kernel, int kw, int kh)                     \
{
    intptr_t h = base->dims[0];
    intptr_t w = base->dims[1];
    uint8_t (*in_data)[w][3];
    in_data = (uint8_t (*)[w][3])NDARRAY_DATAPTR(base);

    NDArray *out = ndarray_new(3, (intptr_t []){h, w, 3}, sizeof(uint8_t), NULL);
    if(!out)
    {
        return NULL;
    }
    
    uint8_t (*out_data)[w][3];
    out_data = (uint8_t (*)[w][3])NDARRAY_DATAPTR(out);
    
    double (*k)[kw] = (double (*)[kw])kernel;
    int jrange = kh/2;
    int irange = kw/2;

    // handle literal edge cases where kernel extends outside of data array by clamping values 
    // top 
    for(int y = 0; y < jrange; y++)
    {
        for(int x = irange; x < (w - irange); x++)
        {
            double val[3] = {0.0};
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((y+j) < 0) ? 0 : (y+j);
                for(int i = -irange; i <= irange; i++)
                {
                    for(int z = 0; z < 3; z++)
                    {
                        val[z] += (double)in_data[yidx][x+i][z] * k[j+jrange][i+irange];
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }
        }
    }

    // bottom 
    for(int y = (h - jrange); y < h; y++)
    {
        for(int x = irange; x < (w - irange); x++)
        {
            double val[3] = {0.0};
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((y+j) > (h-1)) ? (h-1) : (y+j);
                for(int i = -irange; i <= irange; i++)
                {
                    for(int z = 0; z < 3; z++)
                    {
                        val[z] += (double)in_data[yidx][x+i][z] * k[j+jrange][i+irange];
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }            
        }
    }

    // left 
    for(int y = jrange; y < (h - jrange); y++)
    {
        for(int x = 0; x < irange; x++)
        {
            double val[3] = {0.0};
            for(int j = -jrange; j <= jrange; j++)
            {
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((x+i) < 0) ? 0 : (x+i);
                    for(int z = 0; z < 3; z++)
                    {
                        val[z] += (double)in_data[y+j][xidx][z] * k[j+jrange][i+irange];
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }
        }
    }

    // right 
    for(int y = jrange; y < (h - jrange); y++)
    {
        for(int x = (w - irange); x < w; x++)
        {
            double val[3] = {0.0};
            for(int j = -jrange; j <= jrange; j++)
            {
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((x+i) > (w-1)) ? (w-1) : (x+i);
                    for(int z = 0; z < 3; z++)
                    {                    
                        val[z] += (double)in_data[y+j][xidx][z] * k[j+jrange][i+irange];
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }
        }

    }

    // handle 4 corners 
    for(int y = 0; y < jrange; y++)
    {
        for(int x = 0; x < irange; x++)
        {
            // top left 
            //printf("top left corner:\n");
            double val[3] = {0.0};
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((y+j) < 0) ? 0 : (y+j);
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((x+i) < 0) ? 0 : (x+i);
                    for(int z = 0; z < 3; z++)
                    {
                        val[z] += (double)in_data[yidx][xidx][z] * k[j+jrange][i+irange];
                        //printf(" %dx%d = %.2f\n", xidx, yidx, in_data[yidx][xidx] * k[j+jrange][i+irange]);
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }

            // x,y coordinates for bottom right corner 
            int cx = (w - irange) + x;
            int cy = (h - jrange) + y;
            
            // top right 
            //printf("top right corner:\n");
            val[0] = val[1] = val[2] = 0.0;
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((y+j) < 0) ? 0 : (y+j);
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((cx+i) > (w-1)) ? (w-1) : (cx+i);
                    for(int z = 0; z < 3; z++)
                    {                    
                        val[z] += (double)in_data[yidx][xidx][z] * k[j+jrange][i+irange];
                        //printf(" %dx%d\n", xidx, yidx);
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }
            
            // bottom left 
            //printf("bottom left corner:\n");
            val[0] = val[1] = val[2] = 0.0;
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((cy+j) > (h-1)) ? (h-1) : (cy+j);
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((x+i) < 0) ? 0 : (x+i);
                    for(int z = 0; z < 3; z++)
                    {                                        
                        val[z] += (double)in_data[yidx][xidx][z] * k[j+jrange][i+irange];
                        //printf(" %dx%d\n", xidx, yidx);
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }
            
            // bottom right 
            //printf("bottom right corner:\n");
            val[0] = val[1] = val[2] = 0.0;
            for(int j = -jrange; j <= jrange; j++)
            {
                int yidx = ((cy+j) > (h-1)) ? (h-1) : (cy+j);
                for(int i = -irange; i <= irange; i++)
                {
                    int xidx = ((cx+i) > (w-1)) ? (w-1) : (cx+i);
                    for(int z = 0; z < 3; z++)
                    {                                        
                        val[z] += (double)in_data[yidx][xidx][z] * k[j+jrange][i+irange];
                        //printf(" %dx%d\n", xidx, yidx);
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }
        }
    }
    
    // the rest 
    _Pragma("omp parallel for")
    for(int y = jrange; y < (h - jrange); y++)
    {
        for(int x = irange; x < (w - irange); x++)
        {
            double val[3] = {0.0};
            for(int j = -jrange; j <= jrange; j++)
            {
                for(int i = -irange; i <= irange; i++)
                {
                    for(int z = 0; z < 3; z++)
                    {                                                            
                        val[z] += (double)in_data[y+j][x+i][z] * k[j+jrange][i+irange];
                    }
                }
            }
            for(int z = 0; z < 3; z++)
            {
                out_data[y][x][z] = (val[z] < UINT8_MAX) ? (uint8_t)((val[z] < 0.0) ? 0 : val[z]) : UINT8_MAX;
            }
        }
    }

    return out;
}

*/
