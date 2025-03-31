#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

(require "ndarray-ffi.rkt")

(provide ndarray_fill_double
         ndarray_fill_index_double
         ndarray_mul_double
         ndarray_add_double
         ndarray_iter_mul_double
         ndarray_iter_add_double
         ndarray_mul_double_mp
         ndarray_sum_double)

(define-ndarray ndarray_fill_double (_fun _NDArray-pointer _double -> _void))
(define-ndarray ndarray_fill_index_double (_fun _NDArray-pointer -> _void))

(define-ndarray ndarray_mul_double (_fun _NDArray-pointer _NDArray-pointer -> _NDArray-pointer)
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_double (_fun _NDArray-pointer _NDArray-pointer -> _NDArray-pointer)
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_mul_double (_fun _NDArrayIter-pointer _NDArrayIter-pointer -> _NDArray-pointer)
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_double (_fun _NDArrayIter-pointer _NDArrayIter-pointer -> _NDArray-pointer)
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mul_double_mp (_fun _NDArray-pointer _NDArray-pointer -> _NDArray-pointer)
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_sum_double (_fun _NDArray-pointer -> _double))
