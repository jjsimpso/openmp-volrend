#include <stdint.h>
#include <stdbool.h>
#include <malloc.h>
#include <stdio.h>
#include <math.h>

#include "ndarray.h"
#include "nda_volume.h"

#define ELEMENT(x, y, z, w, h) ((x) + ((y) * (w)) + ((z) * (w) * (h)))

typedef Vec3_double grad_fun(NDArray *v, intptr_t x, intptr_t y, intptr_t z);
typedef Rgba class_fun(int value, Vec3_double gradient, ClassifyInfo *cinfo);
typedef double interp_fun(NDArray *v, Vec3_double *p);

/* calculate the magnitude of the vector in elems */
double mag_double(double *elems, int n)
{
    double sum = 0.0;
    for(int i = 0; i < n; i++)
    {
	sum += elems[i];
    }

    return sqrt(sum);
}

/*************************** gradient functions ***************************/
Vec3_double ndarray_vol_central_diff_uint8_t(NDArray *v, intptr_t x, intptr_t y, intptr_t z)
{
    Vec3_double gradient;
    uint8_t *data = (uint8_t *)NDARRAY_DATAPTR(v);
    intptr_t w = v->dims[2];
    intptr_t h = v->dims[1];

    gradient.x = data[ELEMENT(x-1, y, z, w, h)] - data[ELEMENT(x+1, y, z, w, h)];
    gradient.y = data[ELEMENT(x, y-1, z, w, h)] - data[ELEMENT(x, y+1, z, w, h)];
    gradient.z = data[ELEMENT(x, y, z-1, w, h)] - data[ELEMENT(x, y, z+1, w, h)];
    
    return gradient;
}

/* not sure if I'll use this */
NDArray *ndarray_vol_gradient(NDArray *v, grad_fun *func)
{
    if(v->ndim != 3)
    {
	return NULL;
    }
    
    // allocate result array
    NDArray *g = ndarray_new(4, (intptr_t []){v->dims[0], v->dims[1], v->dims[2], 3}, v->elem_bytes, NULL);
    if(!g)
    {
	return NULL;
    }

    return g;
}

/*************************** classification functions ***************************/
Rgba ndarray_vol_classify_simple_uint8(uint8_t value, Vec3_double gradient, ClassifyInfo *cinfo)
{
    Rgba color = {.r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0};
    
    for(int i = 0; i < cinfo->num_mat; i++)
    {
	Material *mat = &(cinfo->mat[i]);
	if((value >= mat->min) && (value <= mat->max))
	{
	    color.r = mat->r;
	    color.g = mat->g;
	    color.b = mat->b;
	    color.a = mat->a;
	    break;
	}
    }
    
    return color;
}

/*************************** interpolation functions ***************************/
double ndarray_vol_interp_nearest_uint8(NDArray *v, Vec3_double *p)
{
    uint8_t *data = (uint8_t *)NDARRAY_DATAPTR(v);
    intptr_t w = v->dims[2];
    intptr_t h = v->dims[1];

    intptr_t x = (uint8_t)p->x + 0.5;
    intptr_t y = (uint8_t)p->y + 0.5;
    intptr_t z = (uint8_t)p->z + 0.5;
    
    return (double)data[ELEMENT(x, y, z, w, h)];
}

double ndarray_vol_interp_linear_uint8(NDArray *v, Vec3_double *p)
{
    uint8_t *data = (uint8_t *)NDARRAY_DATAPTR(v);
    intptr_t w = v->dims[2];
    intptr_t h = v->dims[1];

    intptr_t x = (int)p->x;
    intptr_t y = (int)p->y;
    intptr_t z = (int)p->z;
    
    double dx = p->x - x;
    double dy = p->y - y;
    double dz = p->z - z;

    /* interpolated values 1 and 2 are interpolated across the x-axis in the plane below the sample */
    double iv1 = data[ELEMENT(x-1, y-1, z-1, w, h)];
    iv1 +=      (data[ELEMENT(x+1, y-1, z-1, w, h)] - iv1) * dx;
    double iv2 = data[ELEMENT(x-1, y-1, z+1, w, h)];
    iv2 +=      (data[ELEMENT(x+1, y-1, z+1, w, h)] - iv2) * dx;

    /* interpolated values 3 and 4 are interpolated across the x-axis in the plane above the sample */
    double iv3 = data[ELEMENT(x-1, y+1, z-1, w, h)];
    iv3 +=      (data[ELEMENT(x+1, y+1, z-1, w, h)] - iv3) * dx;
    double iv4 = data[ELEMENT(x-1, y+1, z+1, w, h)];
    iv4 +=      (data[ELEMENT(x+1, y+1, z+1, w, h)] - iv4) * dx;

    /* interpolate between 1 and 3 and between 2 and 4 */
    double iv5 = ((iv3 - iv1) * dy) + iv1;
    double iv6 = ((iv4 - iv2) * dy) + iv2;
    
    /* interpolate between 5 and 6 */
    double final = ((iv6 - iv5) * dz) + iv5;
    
    return final;
}


NDArray *ndarray_vol_render_uint8_t(NDArray *v, int image_width, int image_height, int samples, NDArray *trans, grad_fun *grad, class_fun *classify, ClassifyInfo *cinfo, interp_fun *interpolate)
{
    // 2D RGB image 
    NDArray *out = ndarray_new(3, (intptr_t []){image_height, image_width, 3}, 1, NULL);

    if(!out)
    {
	return NULL;
    }

    uint8_t *data = (uint8_t *)NDARRAY_DATAPTR(v);
    intptr_t w = v->dims[2];
    intptr_t h = v->dims[1];
    //uint8_t val = data[ELEMENT(x, y, z, w, h)];
    
    return out;
}
