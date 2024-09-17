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
    NDArray *nda = ndarray_new(2, (intptr_t []){10, 5}, sizeof(int));

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

    write_ppm("out.ppm", w, h, rgb);
}

int main(int argc, char **argv)
{

    test_simple_iterator();

    test_ppm();
    
    return 0;
}
