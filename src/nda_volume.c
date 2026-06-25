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

/* calculate the magnitude of the vector in elems */
double mag_double(double *elems, int n)
{
    double sum = 0.0;
    for(int i = 0; i < n; i++)
    {
	sum += elems[i] * elems[i];
    }

    return sqrt(sum);
}

/* destructive */
void vec3_normalize_double(Vec3_double *v)
{
    double mag = sqrt((v->x * v->x) + (v->y * v->y) + (v->z * v->z));

    if(mag > 0.0)
    {
	v->x /= mag;
	v->y /= mag;
	v->z /= mag;
    }
}

/* destructive */
void vec4_normalize_double(Vec4_double *v)
{
    double mag = sqrt((v->x * v->x) + (v->y * v->y) + (v->z * v->z));

    if(mag > 0.0)
    {    
	v->x /= mag;
	v->y /= mag;
	v->z /= mag;
    }
}

void vec4_matmul(Mat4x4_double m, Vec4_double *v, Vec4_double *result)
{
    result->x = (m[0][0] * v->x) + (m[0][1] * v->y) + (m[0][2] * v->z) + (m[0][3] * v->w);
    result->y = (m[1][0] * v->x) + (m[1][1] * v->y) + (m[1][2] * v->z) + (m[1][3] * v->w);
    result->z = (m[2][0] * v->x) + (m[2][1] * v->y) + (m[2][2] * v->z) + (m[2][3] * v->w);
    result->w = (m[3][0] * v->x) + (m[3][1] * v->y) + (m[3][2] * v->z) + (m[3][3] * v->w);
}

void mat4x4mul(Mat4x4_double a, Mat4x4_double b, Mat4x4_double c)
{

}

uint8_t clamp_double(double v)
{
    if(v < 0.0)
    {
	return 0;
    }
    else if(v > 1.0)
    {
	return 255;
    }
    else
    {
	return v * 255;
    }
}

bool slab_intersect(Vec4_double *o, Vec4_double *d_inv, Vec3_double *min, Vec3_double *max, double *tnear, double *tfar)
{
    double t1, t2, t3, t4, t5, t6;
    t1 = (min->x - o->x) * d_inv->x;
    t2 = (max->x - o->x) * d_inv->x;
    t3 = (min->y - o->y) * d_inv->y;
    t4 = (max->y - o->y) * d_inv->y;
    t5 = (min->z - o->z) * d_inv->z;
    t6 = (max->z - o->z) * d_inv->z;

    double tmin, tmax;
    tmin = fmax(fmin(t1, t2), fmax(fmin(t3, t4), fmin(t5, t6)));
    tmax = fmin(fmax(t1, t2), fmin(fmax(t3, t4), fmax(t5, t6)));

    if(tmin < tmax)
    {
	*tnear = tmin;
	*tfar = tmax;
	return true;
    }

    return false;
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
Rgba ndarray_vol_classify_simple_uint8_t(uint8_t value, Vec3_double gradient, ClassifyInfo *cinfo)
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
uint8_t ndarray_vol_interp_nearest_uint8_t(NDArray *v, Vec4_double *p)
{
    uint8_t *data = (uint8_t *)NDARRAY_DATAPTR(v);
    intptr_t w = v->dims[2];
    intptr_t h = v->dims[1];

    intptr_t x = (uint8_t)p->x + 0.5;
    intptr_t y = (uint8_t)p->y + 0.5;
    intptr_t z = (uint8_t)p->z + 0.5;
    
    return (double)data[ELEMENT3D(x, y, z, w, h)];
}

uint8_t ndarray_vol_interp_linear_uint8_t(NDArray *v, Vec4_double *p)
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

/* 
   Simple shading function. Diffuse and ambient light only. Single hard-coded light source.

   Returns intensity.
*/
double shade_simple(Vec3_double gradient)
{
    Vec3_double light = { .x = 2.0, .y = -1.25, .z = 2.0 };
    double ambient = 0.1;
    double diffuse = 0.9;

    vec3_normalize_double(&gradient);
    vec3_normalize_double(&light);

    double dot = (gradient.x * light.x) + (gradient.y * light.y) + (gradient.z * light.z);
    if(dot < 0.0) dot = 0.0;

    return ambient + (diffuse * dot);
}

/* 
   Calculate maximum intensity projection of the volume 'v'
   Uses a hard-coded orthographic projection
   'image_width', 'image_height', and 'samples' will generally match the dimensions of the volume
   'trans' is a 4x4 transformation matrix from object to view space
*/
NDArray *ndarray_vol_mip_uint8_t(NDArray *v, int image_width, int image_height, int samples, NDArray *trans)
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
    
    /* find the min/max range of the cube in obj space */
    int minx = 0;
    int maxx = w - 1;
    int miny = 0;
    int maxy = h - 1;
    int minz = 0;
    int maxz = d - 1;

    /* inverse transform to go from view space to object/voxel space */
    NDArray *inv_trans = ndarray_mat_inverse_double(trans);

    /* 
       cast a ray in view space from each pixel on the viewing plane
       translate the ray to obj space and calculate intersection with volume in obj space
       step along ray in obj space
    */
    Vec4_double ray_o = { .x = 0.0, .y = 0.0, .z = 1024.0, .w = 1.0 };
    Vec4_double ray_d = { .x = 0.0, .y = 0.0, .z = -1.0, .w = 0.0 };
    Vec4_double ray_obj_o = { 0.0 };
    Vec4_double ray_obj_d = { 0.0 };
    uint8_t val, maxval;

    /* calculate ray direction in object space and normalize it */
    vec4_matmul((double (*)[4])inv_trans->dataptr, &ray_d, &ray_obj_d);
    vec4_normalize_double(&ray_obj_d);

    /* precalculate 1/d to save on division ops */
    Vec4_double ray_obj_d_inv = { .x = 1.0 / ray_obj_d.x, .y = 1.0 / ray_obj_d.y, .z = 1.0 / ray_obj_d.z, .w = 1.0 };
    
    /* step along image in y direction (rows) */
    for(int j = 0; j < image_height; j++)
    {
	ray_o.y = (double)j;
	
	/* step along image in x direction (columns) */
	for(int i = 0; i < image_width; i++)
	{
	    val = maxval = 0;

	    ray_o.x = (double)i;
	    vec4_matmul((double (*)[4])inv_trans->dataptr, &ray_o, &ray_obj_o);

	    /* calc intersection in object space */
	    Vec3_double min = { minx, miny, minz };
	    Vec3_double max = { maxx, maxy, maxz };
	    double tnear, tfar;

	    if(slab_intersect(&ray_obj_o, &ray_obj_d_inv, &min, &max, &tnear, &tfar))
	    {
		/* point of intersection where ray exits volume (we are compositing back to front) */
		Vec4_double obj_pos;
		obj_pos.x = ray_obj_o.x + tfar * ray_obj_d.x;
		obj_pos.y = ray_obj_o.y + tfar * ray_obj_d.y;
		obj_pos.z = ray_obj_o.z + tfar * ray_obj_d.z;
		obj_pos.w = 1.0;

		//printf("obj_pos = (%f, %f, %f)\n", obj_pos.x, obj_pos.y, obj_pos.z);
		
		/* step back along ray towards front */
		while( ((intptr_t)obj_pos.x >= 0) && ((intptr_t)obj_pos.x <= (w - 1)) &&
		       ((intptr_t)obj_pos.y >= 0) && ((intptr_t)obj_pos.y <= (h - 1)) &&
		       ((intptr_t)obj_pos.z >= 0) && ((intptr_t)obj_pos.z <= (d - 1)) )
		{
		    val = data[ELEMENT3D((intptr_t)obj_pos.x, (intptr_t)obj_pos.y, (intptr_t)obj_pos.z, w, h)];
		    if(val > maxval) maxval = val;
		    
		    obj_pos.x -= ray_obj_d.x;
		    obj_pos.y -= ray_obj_d.y;
		    obj_pos.z -= ray_obj_d.z;
		}
	    }

	    /* set pixel even if the ray didn't intersect */
	    int idx = ELEMENT2D((intptr_t)j, (intptr_t)i, image_width) * 3;
	    out_data[idx++] = maxval;
	    out_data[idx++] = maxval;
	    out_data[idx++] = maxval;
	}
    }
    
    return out;
}

/* 
   Calculate a volume rendering of the volume 'v'
   Uses a hard-coded orthographic projection
   'image_width', 'image_height', and 'samples' will generally match the dimensions of the volume
   'trans' is a 4x4 transformation matrix from object to view space
*/
NDArray *ndarray_vol_render_uint8_t(NDArray *v, int image_width, int image_height, int samples, NDArray *trans, grad_fun *grad, class_fun *classify, ClassifyInfo *cinfo, interp_fun *interpolate)
{
    // 2D RGB image 
    NDArray *out = ndarray_new(3, (intptr_t []){image_height, image_width, 3}, 1, NULL);

    if(!out)
    {
	return NULL;
    }

    intptr_t w = v->dims[2];
    intptr_t h = v->dims[1];
    intptr_t d = v->dims[0];

    uint8_t *out_data = (uint8_t *)NDARRAY_DATAPTR(out);
    
    /* 
       set the min/max range of the cube in obj space 
       use values one voxel inside the bounds of the cube to prevent sampling outside the memory block 
    */
    int minx = 1;
    int maxx = w - 2;
    int miny = 1;
    int maxy = h - 2;
    int minz = 1;
    int maxz = d - 2;

    /* inverse transform to go from view space to object/voxel space */
    NDArray *inv_trans = ndarray_mat_inverse_double(trans);

    /* 
       cast a ray in view space from each pixel on the viewing plane
       translate the ray to obj space and calculate intersection with volume in obj space
       step along ray in obj space
    */
    Vec4_double ray_o = { .x = 0.0, .y = 0.0, .z = 1024.0, .w = 1.0 };
    Vec4_double ray_d = { .x = 0.0, .y = 0.0, .z = -1.0, .w = 0.0 };
    Vec4_double ray_obj_o = { 0.0 };
    Vec4_double ray_obj_d = { 0.0 };

    /* per voxel working values */
    Vec3_double gradient;
    uint8_t val;
    double attenuate;
    Rgba c;
    
    /* calculate ray direction in object space and normalize it */
    vec4_matmul((double (*)[4])inv_trans->dataptr, &ray_d, &ray_obj_d);
    vec4_normalize_double(&ray_obj_d);

    /* precalculate 1/d to save on division ops */
    Vec4_double ray_obj_d_inv = { .x = 1.0 / ray_obj_d.x, .y = 1.0 / ray_obj_d.y, .z = 1.0 / ray_obj_d.z, .w = 1.0 };
    
    /* step along image in y direction (rows) */
    for(int j = 0; j < image_height; j++)
    {
	ray_o.y = (double)j;
	
	/* step along image in x direction (columns) */
	for(int i = 0; i < image_width; i++)
	{
	    ray_o.x = (double)i;
	    vec4_matmul((double (*)[4])inv_trans->dataptr, &ray_o, &ray_obj_o);

	    /* default to black */
	    Rgba color = { .r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0 };
	    
	    /* calc intersection in object space */
	    Vec3_double min = { minx, miny, minz };
	    Vec3_double max = { maxx, maxy, maxz };
	    double tnear, tfar;

	    if(slab_intersect(&ray_obj_o, &ray_obj_d_inv, &min, &max, &tnear, &tfar))
	    {
		/* point of intersection where ray exits volume (we are compositing back to front) */
		Vec4_double obj_pos;
		obj_pos.x = ray_obj_o.x + tfar * ray_obj_d.x;
		obj_pos.y = ray_obj_o.y + tfar * ray_obj_d.y;
		obj_pos.z = ray_obj_o.z + tfar * ray_obj_d.z;
		obj_pos.w = 1.0;

		//printf("obj_pos = (%f, %f, %f)\n", obj_pos.x, obj_pos.y, obj_pos.z);
		
		/* step back along ray towards front while sample point is within bounds of data. */
		while( ((intptr_t)obj_pos.x > 0) && ((intptr_t)obj_pos.x < (w - 1)) &&
		       ((intptr_t)obj_pos.y > 0) && ((intptr_t)obj_pos.y < (h - 1)) &&
		       ((intptr_t)obj_pos.z > 0) && ((intptr_t)obj_pos.z < (d - 1)) )
		{
		    /* perform classification and shading operations per voxel */
		    val = interpolate(v, &obj_pos);
		    gradient = grad(v, obj_pos.x + 0.5, obj_pos.y + 0.5, obj_pos.z + 0.5);
		    c = classify(val, gradient, cinfo);
		    //val = ndarray_vol_interp_linear_uint8_t(v, &obj_pos);
		    //gradient = ndarray_vol_central_diff_uint8_t(v, obj_pos.x + 0.5, obj_pos.y + 0.5, obj_pos.z + 0.5);
		    //c = ndarray_vol_classify_simple_uint8_t(val, gradient, cinfo);
		    attenuate = shade_simple(gradient);

		    c.r *= attenuate;
		    c.g *= attenuate;
		    c.b *= attenuate;

		    /* composite back-to-front using opacity(alpha) */
		    color.r = (c.r * c.a) + (color.r * (1.0 - c.a));
		    color.g = (c.g * c.a) + (color.g * (1.0 - c.a));
		    color.b = (c.b * c.a) + (color.b * (1.0 - c.a));
		    
		    obj_pos.x -= ray_obj_d.x;
		    obj_pos.y -= ray_obj_d.y;
		    obj_pos.z -= ray_obj_d.z;
		}
	    }

	    /* set pixel even if the ray didn't intersect */
	    int idx = ELEMENT2D((intptr_t)j, (intptr_t)i, image_width) * 3;
	    out_data[idx++] = clamp_double(color.r);
	    out_data[idx++] = clamp_double(color.g);
	    out_data[idx++] = clamp_double(color.b);
	}
    }
    
    return out;
}
