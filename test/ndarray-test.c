#include <stdio.h>
#include "ndarray.h"
#include "ppm.h"

void print_iter(NDArrayIter *it)
{
    int i = 0;
    do
    {
	printf("it[%d]=%d\n", i++, (int)ITER_DATA(it));
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
	ITER_LVAL(it) = ((i / NDARRAY_DIM_SIZE(nda, 1)) * 10) + (i % NDARRAY_DIM_SIZE(nda, 1));
	i++;
    } while(ndarray_iter_next(it));

    ndarray_iter_reset(it);
    print_iter(it);

    printf("---- showing slice of ndarray ----\n");
    NDArrayIter *it2 = ndarray_iter_new(nda, (Slice []){ {1,8,3}, {0,2,2} } );
    print_iter(it2);
    
    ndarray_free(nda);
}

void test_ppm()
{
    int w, h;
    uint8_t *rgb = read_ppm("image-01.ppm", &w, &h);

    NDArray *nda = ndarray_new(3, (intptr_t []){h, w, 3}, 1, rgb);

    NDArrayIter *it = ndarray_iter_new(nda, NULL);

    //write_ppm("out.ppm", w, h, rgb);
    ndarray_iter_write_ppm(it, "out.ppm", w, h);
    ndarray_iter_free(it);
    
    int skip_dim = -1;
    NDArrayIter *it2 = ndarray_iter_new_all_but_axis(nda, NULL, &skip_dim);
    ndarray_iter_write_ppm(it2, "out2.ppm", w, h);
    ndarray_iter_free(it2);

    // create an iterator that decimates by 2 in x and y dimensions
    NDArrayIter *it_dec = ndarray_iter_new(nda, (Slice []){{0, (h-1), 2}, {0, (w-1), 2}, {0, 2, 1} });
    ndarray_iter_write_ppm(it_dec, "out3.ppm", w/2, h/2);
    ndarray_iter_free(it_dec);
}

int main(int argc, char **argv)
{

    test_simple_iterator();

    test_ppm();
    
    return 0;
}
