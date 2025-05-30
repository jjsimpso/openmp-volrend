#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

(require "ndarray-ffi.rkt")

(provide ndarray_fill_mat_ident_double
         ndarray_matmul_double)

(define-ndarray ndarray_fill_mat_ident_double (_fun _NDArray-pointer -> _stdbool))

(define-ndarray ndarray_matmul_double (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_mul_double))
  #:wrap (allocator ndarray_free))
