#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

(require "ndarray-ffi.rkt")

(provide ndarray_fill_mat_ident_float
         ndarray_fill_mat_ident_double
         ndarray_fill_mat_ident_int8_t
         ndarray_fill_mat_ident_int16_t
         ndarray_fill_mat_ident_int32_t
         ndarray_fill_mat_ident_int64_t
         ndarray_fill_mat_ident_uint8_t
         ndarray_fill_mat_ident_uint16_t
         ndarray_fill_mat_ident_uint32_t
         ndarray_fill_mat_ident_uint64_t
         ndarray_matmul_float
         ndarray_matmul_double
         ndarray_matmul_int8_t
         ndarray_matmul_int16_t
         ndarray_matmul_int32_t
         ndarray_matmul_int64_t
         ndarray_matmul_uint8_t
         ndarray_matmul_uint16_t
         ndarray_matmul_uint32_t
         ndarray_matmul_uint64_t)

(define-ndarray ndarray_fill_mat_ident_float (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_double (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_int8_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_int16_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_int32_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_int64_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_uint8_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_uint16_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_uint32_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_uint64_t (_fun _NDArray-pointer -> _stdbool))


(define-ndarray ndarray_matmul_float (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_float))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_double (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_double))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_int8_t (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_int8_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_int16_t (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_int16_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_int32_t (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_int32_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_int64_t (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_int64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_uint8_t (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_uint8_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_uint16_t (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_uint16_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_uint32_t (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_uint32_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_uint64_t (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_uint64_t))
  #:wrap (allocator ndarray_free))
