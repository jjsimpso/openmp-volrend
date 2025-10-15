#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <math.h>
#include <complex.h>
#include "ndarray.h"
#include "nda_ops.h"
#include "ppm.h"

void print_iter(NDArrayIter *it)
{
    int i = 0;
    do
    {
	printf("it[%d]=%d\n", i++, ITER_DATA(it, int));
    } while(ndarray_iter_next(it));
}

void print_iter_byte(NDArrayIter *it)
{
    int i = 0;
    do
    {
	printf("it[%d]=%d\n", i++, ITER_DATA(it, uint8_t));
    } while(ndarray_iter_next(it));
}

void print_iter_double(NDArrayIter *it)
{
    int i = 0;
    do
    {
	printf("it[%d]=%.2f\n", i++, (double)ITER_DATA(it, double));
    } while(ndarray_iter_next(it));
}

void print_iter_complex(NDArrayIter *it)
{
    int i = 0;
    do
    {
	printf("it[%d]=%.2f + %.2fi\n", i++, creal((complex)ITER_DATA(it, complex)), cimag((complex)ITER_DATA(it, complex)));
    } while(ndarray_iter_next(it));
}

void test_simple_iterator()
{
    NDArray *nda = ndarray_new(2, (intptr_t []){10, 5}, sizeof(int), NULL);

    printf("created ndarray with dimensions %ldx%ld, %ld elements\n", nda->dims[0], nda->dims[1], nda->num_elems);

    NDArrayIter *it = ndarray_iter_new(nda, NULL);
    int i = 0;
    do
    {
	/* 
	   00 01 02 03 04
           10 11 12 13 14
	   ...
	*/
	ITER_LVAL(it, int) = ((i / NDARRAY_DIM_SIZE(nda, 1)) * 10) + (i % NDARRAY_DIM_SIZE(nda, 1));
	i++;
    } while(ndarray_iter_next(it));

    ndarray_iter_reset(it);
    print_iter(it);

    printf("---- showing slice of ndarray ----\n");
    NDArrayIter *it2 = ndarray_iter_new(nda, (Slice []){ {1,8,3}, {0,2,2} } );
    print_iter(it2);
    
    ndarray_free(nda);
    ndarray_iter_free(it);
    ndarray_iter_free(it2);
}

void test_ppm()
{
    int w, h;
    uint8_t *rgb = read_ppm("image-01.ppm", &w, &h);

    NDArray *nda = ndarray_new(3, (intptr_t []){h, w, 3}, 1, rgb);

    NDArrayIter *it = ndarray_iter_new(nda, NULL);

    //write_ppm("out.ppm", w, h, rgb);
    ndarray_iter_write_ppm(it, "out.ppm", w, h);
    ndarray_iter_reset(it);
    print_iter_byte(it);
    ndarray_iter_free(it);

    // this doesn't work as originally intended now that ndarray_iter_write_file only
    // writes elements matching the actual ndarray element size
    // this was a bit of a hack to begin with
    int skip_dim = -1;
    NDArrayIter *it2 = ndarray_iter_new_all_but_axis(nda, NULL, &skip_dim);
    ndarray_iter_write_ppm(it2, "out2.ppm", w, h);
    ndarray_iter_free(it2);

    // create an iterator that decimates by 2 in x and y dimensions
    NDArrayIter *it_dec = ndarray_iter_new(nda, (Slice []){{0, (h-1), 2}, {0, (w-1), 2}, {0, 2, 1} });
    ndarray_iter_write_ppm(it_dec, "out3.ppm", w/2, h/2);
    ndarray_iter_free(it_dec);

    ndarray_free(nda);
    free(rgb);
}

void test_multi_iterator()
{
    double adat[4][3] = { {  0.0,  0.0,  0.0 },
		       { 10.0, 10.0, 10.0 },
		       { 20.0, 20.0, 20.0 },
		       { 30.0, 30.0, 30.0 } };		       
    double bdat[3] = {1.0, 2.0, 3.0};
    double *a = malloc(sizeof(adat));
    double *b = malloc(sizeof(bdat));
    memcpy(a, adat, sizeof(adat));
    memcpy(b, bdat, sizeof(bdat));
    NDArray *nda_a = ndarray_new(2, (intptr_t []){4, 3}, sizeof(double), (uint8_t *)a);
    NDArray *nda_b = ndarray_new(1, (intptr_t []){3}, sizeof(double), (uint8_t *)b);
    NDArrayIter *it_a = ndarray_iter_new(nda_a, NULL);
    NDArrayIter *it_b = ndarray_iter_new(nda_b, NULL);
    
    printf("Multi Iteration Tests\n");
    printf("---------------------\n");
    printf("A\n");
    print_iter_double(it_a);
    printf("B\n");
    print_iter_double(it_b);
    
    //NDArrayMultiIter *mit = ndarray_multi_iter_new(2, nda_a, nda_b);
    NDArray *nda_c = ndarray_mul_double(nda_a, nda_b);
    NDArrayIter *it_c = ndarray_iter_new(nda_c, NULL);
    printf("A*B\n");
    print_iter_double(it_c);

    // test scalar broadcasting
    double *scalar = malloc(sizeof(double));
    *scalar = 1.5;
    NDArray *nda_scalar = ndarray_new(1, (intptr_t []){1}, sizeof(double), (uint8_t *)scalar);
    NDArray *nda_d = ndarray_mul_double(nda_a, nda_scalar);
    NDArrayIter *it_d = ndarray_iter_new(nda_d, NULL);
    printf("A*1.5\n");
    print_iter_double(it_d);

    // test scalar broadcasting via slice (slice is one element: A[1][0] )
    NDArrayIter *it_scalar = ndarray_iter_new(nda_a, (Slice []){{1, 1, 1}, {0, 0, 1} });
    ndarray_iter_reset(it_a);
    NDArray *nda_e = ndarray_iter_mul_double(it_a, it_scalar);
    NDArrayIter *it_e = ndarray_iter_new(nda_e, NULL);
    printf("A*10\n");
    print_iter_double(it_e);

    NDArray *nda_3d = ndarray_new(3, (intptr_t []){2, 2, 2}, sizeof(double), NULL);
    ndarray_fill_double(nda_3d, 10.0);
    NDArray *nda_2d = ndarray_new(2, (intptr_t []){2, 2}, sizeof(double), NULL);
    ndarray_fill_index_double(nda_2d);
    NDArray *nda_f = ndarray_mul_double(nda_3d, nda_2d);
    NDArrayIter *it_f = ndarray_iter_new(nda_f, NULL);
    printf("2x2x2 matrix * 2x2\n");
    print_iter_double(it_f);

    ndarray_iter_free(it_a);
    ndarray_iter_free(it_b);
    ndarray_iter_free(it_c);
    ndarray_iter_free(it_scalar);
    ndarray_iter_free(it_d);
    ndarray_iter_free(it_e);
    ndarray_iter_free(it_f);
    
    ndarray_free(nda_a);
    ndarray_free(nda_b);
    ndarray_free(nda_c);
    ndarray_free(nda_scalar);
    ndarray_free(nda_d);
    ndarray_free(nda_e);
    ndarray_free(nda_3d);
    ndarray_free(nda_2d);
    ndarray_free(nda_f);

    free(a);
    free(b);
    free(scalar);
}

void linspace(double start, double stop, int num, bool endpoint)
{
    int num_points = endpoint ? (num + 1) : endpoint;
    
    NDArray *indexes = ndarray_new(1, (intptr_t[]){num_points}, sizeof(double), NULL);
    ndarray_fill_index_double(indexes);

    double *start_data = malloc(sizeof(double));
    *start_data = start;
    double *stop_data = malloc(sizeof(double));
    *stop_data = stop - start;
    double *step_data = malloc(sizeof(double));
    *step_data = 1.0 / (double)num;

    //NDArray *a_start = ndarray_new(1, (intptr_t []){1}, sizeof(double), (uint8_t *)start_data);
    //NDArray *a_span = ndarray_new(1, (intptr_t []){1}, sizeof(double), (uint8_t *)stop_data);
    NDArray *a_step = ndarray_new(1, (intptr_t []){1}, sizeof(double), (uint8_t *)step_data);
    NDArray *result = ndarray_mul_double(a_step, indexes);
    
    printf("result[1] = %.3f\n", ((double *)result->dataptr)[2]);

    ndarray_free(indexes);
    ndarray_free(a_step);
    ndarray_free(result);
    
    free(start_data);
    free(stop_data);
    free(step_data);
}

void test_bigsum(int x)
{
    NDArray *nda = ndarray_new(2, (intptr_t []){x, x}, sizeof(double), NULL);
    ndarray_fill_index_double(nda);
    double sum = ndarray_sum_double(nda);
    printf("sum = %f\n", sum);
    ndarray_free(nda);
}

void test_complex()
{
    complex z = 4.0i;
    printf("z = %f + %fi\n", creal(z), cimag(z));
    z = z + 8.0;
    printf("z = %f + %fi\n", creal(z), cimag(z));
    double r = 3.14;
    z = r + z;
    printf("z = %f + %fi\n", creal(z), cimag(z));
    z = csqrt(-2);
    printf("z = %f + %fi\n", creal(z), cimag(z));

    NDArray *nda = ndarray_new(2, (intptr_t []){2, 2}, sizeof(complex), NULL);
    ndarray_fill_complex(nda, z);
    NDArrayIter *it = ndarray_iter_new(nda, NULL);
    print_iter_complex(it);

    NDArray *nda2 = ndarray_new(1, (intptr_t []){1}, sizeof(complex), NULL);
    ndarray_fill_complex(nda2, 7.0 + z);
    NDArray *result = ndarray_add_complex(nda, nda2);
    NDArrayIter *it2 = ndarray_iter_new(result, NULL);
    print_iter_complex(it2);
}

int main(int argc, char **argv)
{

    test_simple_iterator();

    test_ppm();

    test_multi_iterator();

    linspace(0, 5000, 50, true);

    test_bigsum(10);

    test_complex();
    
    return 0;
}
