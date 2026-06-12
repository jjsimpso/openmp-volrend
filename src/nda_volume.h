typedef double Mat4x4_double[4][4];

typedef struct {
    double x, y, z;
} Vec3_double;

typedef struct {
    double x, y, z, w;
} Vec4_double;

typedef struct {
    double r, g, b, a;
} Rgba;

typedef struct {
    int min, max;
    double r, g, b, a;  // a is opacity
} Material;

typedef struct {
    int num_mat;
    Material *mat;
} ClassifyInfo;

/* volume rendering function pointer types */
typedef Vec3_double grad_fun(NDArray *v, intptr_t x, intptr_t y, intptr_t z);
typedef Rgba class_fun(int value, Vec3_double gradient, ClassifyInfo *cinfo);
typedef double interp_fun(NDArray *v, Vec3_double *p);

/* function prototypes */
Vec3_double ndarray_vol_central_diff_double(NDArray *v, intptr_t x, intptr_t y, intptr_t z);

NDArray *ndarray_vol_gradient_double(NDArray *v, Vec3_double (*func)(NDArray *v, intptr_t x, intptr_t y, intptr_t z));

double mag_double(double *elems, int n);

NDArray *ndarray_vol_mip_uint8_t(NDArray *v, int image_width, int image_height, int samples, NDArray *trans);
NDArray *ndarray_vol_render_uint8_t(NDArray *v, int image_width, int image_height, int samples, NDArray *trans, grad_fun *grad, class_fun *classify, ClassifyInfo *cinfo, interp_fun *interpolate);
