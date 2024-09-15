#include <stdint.h>
#include <malloc.h>
#include <string.h>

#include "ndarray.h"


NDArray *ndarray_new(int n, intptr_t *dims, intptr_t elem_bytes)
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

    nda->dataptr = (uint8_t *) malloc(nda->size);
    if(!(nda->dataptr))
    {
	free(nda);
	return NULL;
    }

    nda->dims = malloc(n * sizeof(intptr_t));
    if(!(nda->dims))
    {
	free(nda->dataptr);
	free(nda);
	return NULL;
    }

    memcpy(nda->dims, dims, n * sizeof(intptr_t));
    
    return nda;
}

void ndarray_free(NDArray *nda)
{
    if(nda)
    {
    	if(nda->dataptr) free(nda->dataptr);
	if(nda->dims) free(nda->dims);
	free(nda);
    }
}

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

	for(int i = 0; i < nda->ndim; i++)
	{
	    iter->dims_m1[i] = nda->dims[i] - 1;
	}
	// for contiguous arrays we only need to set strides[0]
	// it can always be used to iterate to the next element
	iter->strides[0] = nda->elem_bytes;
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

inline bool ndarray_iter_more(NDArrayIter *it)
{
    return (it->index < it->length);
}

// return false if iterator is finished
bool ndarray_iter_next(NDArrayIter *it)
{
    if(it->contiguous)
    {
	it->cursor += it->strides[0];
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
