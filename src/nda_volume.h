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
    double amb, diff, spec;
    double r, g, b, a;  // a is opacity
} Material;

typedef struct {
    int num_mat;
    Material *mat;
    double lev_threshold, lev_width;
} ClassifyInfo;

/* volume rendering function pointer types */
typedef Vec3_double grad_fun(NDArray *v, intptr_t x, intptr_t y, intptr_t z);
typedef Material *class_fun(int value, Vec3_double gradient, ClassifyInfo *cinfo);
typedef uint8_t interp_fun(NDArray *v, Vec4_double *p);

/* gradient function prototypes */
Vec3_double ndarray_vol_central_diff_uint8_t(NDArray *v, intptr_t x, intptr_t y, intptr_t z);

/* classification function prototypes */
Material * ndarray_vol_classify_simple_uint8_t(uint8_t value, Vec3_double gradient, ClassifyInfo *cinfo);

/* interpolation function prototypes */
uint8_t ndarray_vol_interp_nearest_uint8_t(NDArray *v, Vec4_double *p);
uint8_t ndarray_vol_interp_linear_uint8_t(NDArray *v, Vec4_double *p);


NDArray *ndarray_vol_mip_uint8_t(NDArray *v, int image_width, int image_height, int samples, NDArray *trans);
NDArray *ndarray_vol_render_uint8_t(NDArray *v, int image_width, int image_height, int samples, NDArray *trans, double persp_dist, grad_fun *grad, class_fun *classify, ClassifyInfo *cinfo, interp_fun *interpolate);
