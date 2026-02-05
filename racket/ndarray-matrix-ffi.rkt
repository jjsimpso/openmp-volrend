#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

(require "ndarray-ffi.rkt")

(provide ndarray_fill_mat_ident_float
         ndarray_fill_mat_ident_double
         ndarray_fill_mat_ident_complex
         ndarray_fill_mat_ident_int8_t
         ndarray_fill_mat_ident_int16_t
         ndarray_fill_mat_ident_int32_t
         ndarray_fill_mat_ident_int64_t
         ndarray_fill_mat_ident_uint8_t
         ndarray_fill_mat_ident_uint16_t
         ndarray_fill_mat_ident_uint32_t
         ndarray_fill_mat_ident_uint64_t
         ndarray_mat_transpose_float
         ndarray_mat_transpose_double
         ndarray_mat_transpose_complex
         ndarray_mat_transpose_int8_t
         ndarray_mat_transpose_int16_t
         ndarray_mat_transpose_int32_t
         ndarray_mat_transpose_int64_t
         ndarray_mat_transpose_uint8_t
         ndarray_mat_transpose_uint16_t
         ndarray_mat_transpose_uint32_t
         ndarray_mat_transpose_uint64_t
         ndarray_matmul_float
         ndarray_matmul_double
         ndarray_matmul_complex
         ndarray_matmul_int8_t
         ndarray_matmul_int16_t
         ndarray_matmul_int32_t
         ndarray_matmul_int64_t
         ndarray_matmul_uint8_t
         ndarray_matmul_uint16_t
         ndarray_matmul_uint32_t
         ndarray_matmul_uint64_t
         ndarray_iter_matmul_float
         ndarray_iter_matmul_double
         ndarray_iter_matmul_complex
         ndarray_iter_matmul_int8_t
         ndarray_iter_matmul_int16_t
         ndarray_iter_matmul_int32_t
         ndarray_iter_matmul_int64_t
         ndarray_iter_matmul_uint8_t
         ndarray_iter_matmul_uint16_t
         ndarray_iter_matmul_uint32_t
         ndarray_iter_matmul_uint64_t)

(define-ndarray ndarray_fill_mat_ident_float (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_double (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_complex (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_int8_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_int16_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_int32_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_int64_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_uint8_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_uint16_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_uint32_t (_fun _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_fill_mat_ident_uint64_t (_fun _NDArray-pointer -> _stdbool))

(define-ndarray ndarray_mat_transpose_float (_fun _NDArray-pointer
                                                  -> (p : _NDArray-pointer/null)
                                                  -> (check-null p 'ndarray_mat_transpose_float))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_double (_fun _NDArray-pointer
                                                   -> (p : _NDArray-pointer/null)
                                                   -> (check-null p 'ndarray_mat_transpose_double))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_complex (_fun _NDArray-pointer
                                                    -> (p : _NDArray-pointer/null)
                                                    -> (check-null p 'ndarray_mat_transpose_complex))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_int8_t (_fun _NDArray-pointer
                                                   -> (p : _NDArray-pointer/null)
                                                   -> (check-null p 'ndarray_mat_transpose_int8_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_int16_t (_fun _NDArray-pointer
                                                    -> (p : _NDArray-pointer/null)
                                                    -> (check-null p 'ndarray_mat_transpose_int16_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_int32_t (_fun _NDArray-pointer
                                                    -> (p : _NDArray-pointer/null)
                                                    -> (check-null p 'ndarray_mat_transpose_int32_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_int64_t (_fun _NDArray-pointer
                                                    -> (p : _NDArray-pointer/null)
                                                    -> (check-null p 'ndarray_mat_transpose_int64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_uint8_t (_fun _NDArray-pointer
                                                    -> (p : _NDArray-pointer/null)
                                                    -> (check-null p 'ndarray_mat_transpose_uint8_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_uint16_t (_fun _NDArray-pointer
                                                     -> (p : _NDArray-pointer/null)
                                                     -> (check-null p 'ndarray_mat_transpose_uint16_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_uint32_t (_fun _NDArray-pointer
                                                     -> (p : _NDArray-pointer/null)
                                                     -> (check-null p 'ndarray_mat_transpose_uint32_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_mat_transpose_uint64_t (_fun _NDArray-pointer
                                                     -> (p : _NDArray-pointer/null)
                                                     -> (check-null p 'ndarray_mat_transpose_uint64_t))
  #:wrap (allocator ndarray_free))


(define-ndarray ndarray_matmul_float (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_float))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_double (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_double))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_matmul_complex (_fun _NDArray-pointer _NDArray-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_matmul_complex))
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


;; Iterator matmul funcs
(define-ndarray ndarray_iter_matmul_float (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_float))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_double (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_double))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_complex (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_complex))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_int8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_int8_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_int16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_int16_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_int32_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_int32_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_int64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_int64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_uint8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_uint8_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_uint16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_uint16_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_uint32_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_uint32_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_matmul_uint64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                            -> (p : _NDArray-pointer/null)
                                            -> (check-null p 'ndarray_iter_matmul_uint64_t))
  #:wrap (allocator ndarray_free))
