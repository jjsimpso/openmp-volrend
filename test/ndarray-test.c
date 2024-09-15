#include <stdio.h>
#include "ndarray.h"

void print_iter(NDArrayIter *it)
{
    int i = 0;
    do
    {
	printf("it[%d]=%d\n", i++, *(it->cursor));
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
	*(it->cursor) = ((i / nda->dims[1]) * 10) + (i % nda->dims[1]);
	i++;
    } while(ndarray_iter_next(it));

    ndarray_iter_reset(it);
    print_iter(it);

    printf("---- showing slice of ndarray ----\n");
    NDArrayIter *it2 = ndarray_iter_new(nda, (Slice []){ {1,7,3}, {1,3,2} } );
    print_iter(it2);
    
    ndarray_free(nda);
}

int main(int argc, char **argv)
{

    test_simple_iterator();
    
    return 0;
}
