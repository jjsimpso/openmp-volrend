#include <stdint.h>
#include <stdbool.h>
#include <malloc.h>
#include <stdio.h>
#include <math.h>

#include "ndarray.h"
#include "nda_matrix.h"
#include "nda_volume.h"

#define ELEMENT3D(x, y, z, w, h) ((x) + ((y) * (w)) + ((z) * (w) * (h)))

/* index at ith row and jth column */
#define ELEMENT2D(i, j, cols) ((j) + ((i) * (cols)))

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

void vec4_matmul(Mat4x4_double m, Vec4_double *v, Vec4_double *result)
{
    result->x = (m[0][0] * v->x) + (m[0][1] * v->y) + (m[0][2] * v->z) + (m[0][4] * v->w);
    result->y = (m[1][0] * v->x) + (m[1][1] * v->y) + (m[1][2] * v->z) + (m[1][4] * v->w);
    result->z = (m[2][0] * v->x) + (m[2][1] * v->y) + (m[2][2] * v->z) + (m[2][4] * v->w);
    result->w = (m[3][0] * v->x) + (m[3][1] * v->y) + (m[3][2] * v->z) + (m[3][4] * v->w);
}

void mat4x4mul(Mat4x4_double a, Mat4x4_double b, Mat4x4_double c)
{

}

/*************************** gradient functions ***************************/
Vec3_double ndarray_vol_central_diff_uint8_t(NDArray *v, intptr_t x, intptr_t y, intptr_t z)
{
    Vec3_double gradient;
    uint8_t *data = (uint8_t *)NDARRAY_DATAPTR(v);
    intptr_t w = v->dims[2];
    intptr_t h = v->dims[1];

    gradient.x = data[ELEMENT3D(x-1, y, z, w, h)] - data[ELEMENT3D(x+1, y, z, w, h)];
    gradient.y = data[ELEMENT3D(x, y-1, z, w, h)] - data[ELEMENT3D(x, y+1, z, w, h)];
    gradient.z = data[ELEMENT3D(x, y, z-1, w, h)] - data[ELEMENT3D(x, y, z+1, w, h)];
    
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
    
    return (double)data[ELEMENT3D(x, y, z, w, h)];
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
    double iv1 = data[ELEMENT3D(x,   y, z,   w, h)];
    iv1 +=      (data[ELEMENT3D(x+1, y, z,   w, h)] - iv1) * dx;
    double iv2 = data[ELEMENT3D(x,   y, z+1, w, h)];
    iv2 +=      (data[ELEMENT3D(x+1, y, z+1, w, h)] - iv2) * dx;

    /* interpolated values 3 and 4 are interpolated across the x-axis in the plane above the sample */
    double iv3 = data[ELEMENT3D(x,   y+1, z,   w, h)];
    iv3 +=      (data[ELEMENT3D(x+1, y+1, z,   w, h)] - iv3) * dx;
    double iv4 = data[ELEMENT3D(x,   y+1, z+1, w, h)];
    iv4 +=      (data[ELEMENT3D(x+1, y+1, z+1, w, h)] - iv4) * dx;

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
    intptr_t d = v->dims[0];

    uint8_t *out_data = (uint8_t *)NDARRAY_DATAPTR(out);
    
    /* 
       Transform the 8 corners of the volume from object to view space using trans.
       This will determine the bounds of the volume in view space 
    */
    Vec4_double corner_obj[8] = {{.x=0.0, .y=0.0, .z=0.0, .w=1.0}};
    Vec4_double corner_view[8] = {{.x=0.0, .y=0.0, .z=0.0, .w=1.0}};

    /* set y values for top plane of the cube */
    for(int i = 4; i < 8; i++)
    {
	corner_obj[i].y = (double)h - 1.0;
    }

    /* set x and z values where they are != 0 */
    corner_obj[1].x = (double)w - 1.0;
    corner_obj[2].x = (double)w - 1.0;
    corner_obj[2].z = (double)d - 1.0;
    corner_obj[3].z = (double)d - 1.0;

    corner_obj[5].x = (double)w - 1.0;
    corner_obj[6].x = (double)w - 1.0;
    corner_obj[6].z = (double)d - 1.0;
    corner_obj[7].z = (double)d - 1.0;

    /* transform corners to view space */
    for(int i = 0; i < 8; i++)
    {
	vec4_matmul((double (*)[4])trans->dataptr, &corner_obj[i], &corner_view[i]);
    }

    /* find the min/max range of the cube in view space */
    int minx = corner_view[0].x;
    int maxx = corner_view[0].x;
    int miny = corner_view[0].y;
    int maxy = corner_view[0].y;
    int minz = corner_view[0].z;
    int maxz = corner_view[0].z;
    for(int i = 1; i < 8; i++)
    {
	if(corner_view[i].x < minx) minx = corner_view[i].x;
	if(corner_view[i].y < miny) miny = corner_view[i].y;
	if(corner_view[i].z < minz) minz = corner_view[i].z;
	if(corner_view[i].x > maxx) maxx = corner_view[i].x;
	if(corner_view[i].y > maxy) maxy = corner_view[i].y;
	if(corner_view[i].z > maxz) maxz = corner_view[i].z;
    }
    
    maxx++;
    maxy++;
    maxz++;

    /* inverse transform to go from view space to object/voxel space */
    NDArray *inv_trans = ndarray_mat_inverse_double(trans);

    Vec4_double pos = { .x = 0.0, .y = 0.0, .z = 0.0, .w = 1.0 };
    Vec4_double obj_pos = { .x = 0.0, .y = 0.0, .z = 0.0, .w = 1.0 };
    uint8_t val, maxval;
    
    /* step in y direction */
    for(int j = miny; j < maxy; j++)
    {
	pos.y = (double)j;
	
	/* step in x direction */
	for(int i = minx; i < maxx; i++)
	{
	    pos.x = (double)i;

	    bool in_volume = false;
	    val = maxval = 0;
	    
	    /* step along ray in z direction */
	    for(int k = minz; k < maxz; k++)
	    {
		pos.z = (double)k;
		vec4_matmul((double (*)[4])inv_trans->dataptr, &pos, &obj_pos);
		
		if( ((intptr_t)obj_pos.x > 1) && ((intptr_t)obj_pos.x < (w - 1)) &&
		    ((intptr_t)obj_pos.y > 1) && ((intptr_t)obj_pos.y < (h - 1)) &&
		    ((intptr_t)obj_pos.z > 1) && ((intptr_t)obj_pos.z < (d - 1)) )
		{
		    val = data[ELEMENT3D((intptr_t)obj_pos.x, (intptr_t)obj_pos.y, (intptr_t)obj_pos.z, w, h)];
		    if(val > maxval) maxval = val;
		    in_volume = true;
		}
		else
		{
		    if(in_volume) break;
		}
	    }

	    int idx = ELEMENT2D((intptr_t)pos.y, (intptr_t)pos.x, w) * 3;
	    out_data[idx++] = maxval;
	    out_data[idx++] = maxval;
	    out_data[idx++] = maxval;
	}
    }
    
    return out;
}
