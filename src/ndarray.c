#include <stdint.h>
#include <stdarg.h>
#include <malloc.h>
#include <string.h>

#include "ndarray.h"


NDArray *ndarray_new(int n, intptr_t *dims, intptr_t elem_bytes, uint8_t *ptr)
{
    if(n > MAX_DIMS || n <= 0)
    {
	return NULL;
    }
    
    NDArray *nda = (NDArray *) malloc(sizeof(NDArray));

    if(!nda)
    {
	return NULL;
    }

    intptr_t num_elems = dims[0];
    for(int i = 1; i < n; i++)
    {
	num_elems *= dims[i];
    }

    nda->ndim = n;
    nda->num_elems = num_elems;
    nda->elem_bytes = elem_bytes;
    nda->size = num_elems * elem_bytes;
    nda->free_data = false;

    if(ptr)
    {
	nda->dataptr = ptr;
    }
    else
    {
	nda->dataptr = (uint8_t *) malloc(nda->size);
	nda->free_data = true;
	if(!(nda->dataptr))
	{
	    free(nda);
	    return NULL;
	}
    }
    
    nda->dims = malloc(n * sizeof(intptr_t));
    if(!(nda->dims))
    {
	if(!ptr) free(nda->dataptr);
	free(nda);
	return NULL;
    }

    memcpy(nda->dims, dims, n * sizeof(intptr_t));
    
    return nda;
}

// Note: frees the dataptr, which could have been passed into ndarray_new and not allocated by it
void ndarray_free(NDArray *nda)
{
    if(nda)
    {
    	if(nda->free_data && nda->dataptr) free(nda->dataptr);
	if(nda->dims) free(nda->dims);
	free(nda);
    }
}

/* 
   Create a new iterator. If slices is NULL, the iterator will iterate over the whole array.
   
*/
NDArrayIter *ndarray_iter_new(NDArray *nda, Slice *slices)
{
    if(!nda)
    {
	return NULL;
    }

    NDArrayIter *iter = (NDArrayIter *) malloc(sizeof(NDArrayIter));

    if(!iter)
    {
	return NULL;
    }


    iter->nd_m1 = nda->ndim - 1;
    iter->index = 0;
    iter->nda = nda;

    if(!slices)
    {
	iter->contiguous = true;
	iter->length = nda->num_elems;
	iter->cursor = nda->dataptr;

	memset(iter->coords, 0, sizeof(iter->coords));
	memset(iter->dims_m1, 0, sizeof(iter->dims_m1));
	memset(iter->strides, 0, sizeof(iter->strides));
	memset(iter->backstrides, 0, sizeof(iter->backstrides));
	memset(iter->slicestarts, 0, sizeof(iter->slicestarts));

	for(int i = iter->nd_m1; i >= 0; i--)
	{
	    iter->dims_m1[i] = nda->dims[i] - 1;
	    if(i == iter->nd_m1)
	    {
		iter->strides[i] = nda->elem_bytes;
	    }
	    else
	    {
		iter->strides[i] = nda->dims[i+1] * iter->strides[i+1];
	    }
	    iter->backstrides[i] = iter->dims_m1[i] * iter->strides[i];
	}
    }
    else
    {
	intptr_t nda_strides[MAX_DIMS] = {0};
	
	iter->contiguous = false;
	iter->length = 1;
	iter->cursor = nda->dataptr;
	
	for(int i = iter->nd_m1; i >= 0; i--)
	{
	    iter->dims_m1[i] = (slices[i].end - slices[i].start) / slices[i].stride;
	    iter->coords[i] = 0;
	    if(i == iter->nd_m1)
	    {
		nda_strides[i] = nda->elem_bytes;
		iter->strides[i] = slices[i].stride * nda->elem_bytes;
	    }
	    else
	    {
		nda_strides[i] = nda->dims[i+1] * nda_strides[i+1];
		iter->strides[i] = slices[i].stride * nda_strides[i];
	    }
	    iter->backstrides[i] = iter->dims_m1[i] * iter->strides[i];
	    iter->slicestarts[i] = slices[i].start;
	    iter->length *= iter->dims_m1[i] + 1;
	    iter->cursor += slices[i].start * nda_strides[i];
	}
    }
    
    return iter;
}

void ndarray_iter_free(NDArrayIter *it)
{
    free(it);
}

// dim is a pointer to the dimension/axis to skip over in the iterator. first dimension is 0
// if dim points to -1, use the last dimension and set dim's value to it
NDArrayIter *ndarray_iter_new_all_but_axis(NDArray *nda, Slice *slices, int *dim)
{
    if(!nda)
    {
	return NULL;
    }

    NDArrayIter *iter = ndarray_iter_new(nda, slices);

    if(!iter)
    {
	return NULL;
    }

    if(*dim == -1)
    {
	*dim = iter->nd_m1;
    }

    int skip_dim = *dim;
    iter->nd_m1--;;
    iter->length = iter->length / (iter->dims_m1[skip_dim] + 1);
    iter->dims_m1[skip_dim] = 0;
    iter->backstrides[skip_dim] = 0;
    iter->contiguous = 0;

    return iter;
}

// specify the always_inline attribute so this gets inlined even at -O0
// prevents linker errors when debugging at -O0
inline bool ndarray_iter_more(NDArrayIter *it) __attribute__((always_inline));
inline bool ndarray_iter_more(NDArrayIter *it)
{
    return (it->index < it->length);
}

// return false if iterator is finished
bool ndarray_iter_next(NDArrayIter *it)
{
    if(it->contiguous)
    {
	it->cursor += it->nda->elem_bytes;
	it->index++;
    }
    else
    {
	for(int i = it->nd_m1; i >= 0; i--)
	{
	    if(it->coords[i] < it->dims_m1[i])
	    {
		it->coords[i]++;
		it->cursor += it->strides[i];
		break;
	    }
	    else
	    {
		it->coords[i] = 0;
		it->cursor -= it->backstrides[i];
	    }
	}
	it->index++;
    }

    return ndarray_iter_more(it);
    //return (it->index < it->length);
}

void ndarray_iter_reset(NDArrayIter *it)
{
    if(it->contiguous)
    {
	it->cursor = it->nda->dataptr;
	it->index = 0;
    }
    else
    {
	it->index = 0;
	memset(it->coords, 0, it->nda->ndim);
	it->cursor = it->nda->dataptr;
	
	intptr_t nda_strides[MAX_DIMS] = {0};
	for(int i = it->nd_m1; i >= 0; i--)
	{
	    nda_strides[i] = (i == it->nd_m1) ? it->nda->elem_bytes : (it->nda->dims[i+1] * nda_strides[i+1]);
	    it->cursor += it->slicestarts[i] * nda_strides[i];
	}
    }
}

int ndarray_iter_write_file(NDArrayIter *it, FILE *out)
{
    int err = 0;
    int cnt = 0;
    
    if(out)
    {
	if(it->contiguous)
	{
	    int num_bytes = it->nda->elem_bytes;
	    do
	    {
		cnt = fwrite(it->cursor, 1, num_bytes, out);
		if(cnt != num_bytes)
		{
		    err = -2;
		    break;
		}
	    } while(ndarray_iter_next(it));
	}
	else
	{
	    int num_bytes = it->strides[it->nd_m1];
	    do
	    {
		cnt = fwrite(it->cursor, 1, num_bytes, out);
		if(cnt != num_bytes)
		{
		    err = -2;
		    break;
		}
	    } while(ndarray_iter_next(it));
	}
    }
    else
    {
	err = -1;
    }
    
    return err;
}

NDArrayMultiIter *ndarray_multi_iter_new(int num, ...)
{
    va_list ap;

    NDArrayMultiIter *multi_iter = (NDArrayMultiIter *) malloc(sizeof(NDArrayMultiIter));

    if(!multi_iter)
    {
	return NULL;
    }

    multi_iter->nda = (NDArray **) malloc(sizeof(NDArray*) * num);
    if(!multi_iter->nda)
    {
	free(multi_iter);
	return NULL;
    }
    
    multi_iter->iter = (NDArrayIter **) malloc(sizeof(NDArrayIter*) * num);
    if(!multi_iter->iter)
    {
	free(multi_iter->nda);
	free(multi_iter);
	return NULL;
    }

    multi_iter->num = num;
    multi_iter->nd_m1 = 0;
    multi_iter->index = 0;
    multi_iter->length = 1;
    memset(multi_iter->dims_m1, 0, sizeof(multi_iter->dims_m1));
    
    NDArray **nda = multi_iter->nda;
    NDArrayIter **iter = multi_iter->iter;

    va_start(ap, num);
    for(int i = 0; i < num; i++)
    {
	nda[i] = va_arg(ap, NDArray *);
	iter[i] = ndarray_iter_new(nda[i], NULL);

	if(iter[i] == NULL)
	{
	    va_end(ap);
	    free(multi_iter->nda);
	    free(multi_iter->iter);
	    free(multi_iter);	    
	    return NULL;
	}
	
	// set iterator to non-contiguous so that it will use strides and backstrides we calculate here
	iter[i]->contiguous = false;
	
	// determine the multi iterator's output dimension
	if(iter[i]->nd_m1 > multi_iter->nd_m1)
	{
	    multi_iter->nd_m1 = iter[i]->nd_m1;
	}
    }
    va_end(ap);

    // walk through the iterators to figure out the output array's shape
    for(int i = 0; i < num; i++)
    {
	if(iter[i]->nd_m1 < multi_iter->nd_m1)
	{
	    // missing dimensions will be at the beginning and array's data will occupy the the final dimensions.
	    intptr_t diff = multi_iter->nd_m1 - iter[i]->nd_m1;
	    bcopy(&(iter[i]->dims_m1[0]), &(iter[i]->dims_m1[diff]), sizeof(intptr_t) * (iter[i]->nd_m1 + 1));
	    bcopy(&(iter[i]->strides[0]), &(iter[i]->strides[diff]), sizeof(intptr_t) * (iter[i]->nd_m1 + 1));
	    bcopy(&(iter[i]->backstrides[0]), &(iter[i]->backstrides[diff]), sizeof(intptr_t) * (iter[i]->nd_m1 + 1));

	    // fill in missing dimensions with broadcast dimensions and 0 strides
	    for(int j = 0; j < diff; j++)
	    {
		iter[i]->dims_m1[j] = 0; //mult_iter->dims_m1[j];
		iter[i]->strides[j] = 0;
		iter[i]->backstrides[j] = 0;
	    }

	    iter[i]->nd_m1 = multi_iter->nd_m1;
	}

	for(int j = 0; j <= multi_iter->nd_m1; j++)
	{
	    if(iter[i]->dims_m1[j] != 0)
	    {
		if(iter[i]->dims_m1[j] != multi_iter->dims_m1[j])
		{
		    // size of each dimension must match or be equal to 1
		    if(multi_iter->dims_m1[j] == 0)
		    {
			// set the output dimension size
			multi_iter->dims_m1[j] = iter[i]->dims_m1[j];
			multi_iter->length *= iter[i]->dims_m1[j] + 1;
		    }
		    else
		    {
			free(multi_iter->nda);
			free(multi_iter->iter);
			free(multi_iter);
			return NULL;
		    }
		}
	    }
	}
    }

    return multi_iter;
}

NDArrayMultiIter *ndarray_multi_iter_new_from_iter(int num, ...)
{
    va_list ap;

    NDArrayMultiIter *multi_iter = (NDArrayMultiIter *) malloc(sizeof(NDArrayMultiIter));

    if(!multi_iter)
    {
	return NULL;
    }

    multi_iter->nda = (NDArray **) malloc(sizeof(NDArray*) * num);
    if(!multi_iter->nda)
    {
	free(multi_iter);
	return NULL;
    }
    
    multi_iter->iter = (NDArrayIter **) malloc(sizeof(NDArrayIter*) * num);
    if(!multi_iter->iter)
    {
	free(multi_iter->nda);
	free(multi_iter);
	return NULL;
    }

    multi_iter->num = num;
    multi_iter->nd_m1 = 0;
    multi_iter->index = 0;
    multi_iter->length = 1;
    memset(multi_iter->dims_m1, 0, sizeof(multi_iter->dims_m1));
    
    NDArray **nda = multi_iter->nda;
    NDArrayIter **iter = multi_iter->iter;

    va_start(ap, num);
    for(int i = 0; i < num; i++)
    {
	iter[i] = va_arg(ap, NDArrayIter *);
	nda[i] = iter[i]->nda;

	// set iterator to non-contiguous so that it will use strides and backstrides we calculate here
	iter[i]->contiguous = false;
	
	// determine the multi iterator's output dimension
	if(iter[i]->nd_m1 > multi_iter->nd_m1)
	{
	    multi_iter->nd_m1 = iter[i]->nd_m1;
	}
    }
    va_end(ap);

    // walk through the iterators to figure out the output array's shape
    for(int i = 0; i < num; i++)
    {
	if(iter[i]->nd_m1 < multi_iter->nd_m1)
	{
	    // missing dimensions will be at the beginning and array's data will occupy the the final dimensions.
	    intptr_t diff = multi_iter->nd_m1 - iter[i]->nd_m1;
	    bcopy(&(iter[i]->dims_m1[0]), &(iter[i]->dims_m1[diff]), sizeof(intptr_t) * (iter[i]->nd_m1 + 1));
	    bcopy(&(iter[i]->strides[0]), &(iter[i]->strides[diff]), sizeof(intptr_t) * (iter[i]->nd_m1 + 1));
	    bcopy(&(iter[i]->backstrides[0]), &(iter[i]->backstrides[diff]), sizeof(intptr_t) * (iter[i]->nd_m1 + 1));
	    
	    // fill in missing dimensions with broadcast dimensions and 0 strides
	    for(int j = 0; j < diff; j++)
	    {
		iter[i]->dims_m1[j] = 0; //mult_iter->dims_m1[j];
		iter[i]->strides[j] = 0;
		iter[i]->backstrides[j] = 0;
	    }

	    iter[i]->nd_m1 = multi_iter->nd_m1;
	}

	for(int j = 0; j <= multi_iter->nd_m1; j++)
	{
	    if(iter[i]->dims_m1[j] != 0)
	    {
		if(iter[i]->dims_m1[j] != multi_iter->dims_m1[j])
		{
		    // size of each dimension must match or be equal to 1
		    if(multi_iter->dims_m1[j] == 0)
		    {
			// set the output dimension size
			multi_iter->dims_m1[j] = iter[i]->dims_m1[j];
			multi_iter->length *= iter[i]->dims_m1[j] + 1;
		    }
		    else
		    {
			free(multi_iter->nda);
			free(multi_iter->iter);
			free(multi_iter);
			return NULL;
		    }
		}
	    }
	}
    }

    return multi_iter;
}

/* 

   Warning: this will also free any iterators passed into ndarray_multi_iter_new_from_iter
*/
void ndarray_multi_iter_free(NDArrayMultiIter *mit)
{
    for(int i = 0; i < mit->num; i++)
    {
	ndarray_iter_free(mit->iter[i]);
    }
    
    free(mit->nda);
    free(mit->iter);
    free(mit);
}

/* 
   Frees the multi iterator struct but doesn't free the iterators used by it 
*/
void ndarray_multi_iter_free_except_iter(NDArrayMultiIter *mit)
{
    free(mit->nda);
    free(mit->iter);
    free(mit);
}

bool ndarray_multi_iter_next(NDArrayMultiIter *mit)
{
    
    for(int i = 0; i < mit->num; i++)
    {
	NDArrayIter *it = mit->iter[i];

	// advance iterator to next element by looping through the dimensions of the iterator
	for(int j = it->nd_m1; j >= 0; j--)
	{
	    if(it->coords[j] < it->dims_m1[j])
	    {
		it->coords[j]++;
		it->cursor += it->strides[j];
		break;
	    }
	    else
	    {
		it->coords[j] = 0;
		it->cursor -= it->backstrides[j];
	    }
	}
	it->index++;
    }

    mit->index++;
    
    return (mit->index < mit->length);
}
