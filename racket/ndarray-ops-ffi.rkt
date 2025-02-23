#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

(require "ndarray-ffi.rkt")

(provide ndarray_fill_double
         ndarray_fill_index_double
         ndarray_mul_double
         ndarray_add_double)

(define-ndarray ndarray_fill_double (_fun _ndarray-pointer _double -> _void))
(define-ndarray ndarray_fill_index_double (_fun _ndarray-pointer -> _void))

(define-ndarray ndarray_mul_double (_fun _ndarray-pointer _ndarray-pointer -> _ndarray-pointer)
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_double (_fun _ndarray-pointer _ndarray-pointer -> _ndarray-pointer)
  #:wrap (allocator ndarray_free))
