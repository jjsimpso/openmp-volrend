#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

(require "ndarray-ffi.rkt")

(provide ndarray_fill_float
         ndarray_fill_double
         ndarray_fill_int8_t
         ndarray_fill_int16_t 
         ndarray_fill_int32_t 
         ndarray_fill_int64_t 
         ndarray_fill_uint8_t 
         ndarray_fill_uint16_t
         ndarray_fill_uint32_t
         ndarray_fill_uint64_t
         ndarray_fill_index_float
         ndarray_fill_index_double
         ndarray_fill_index_int8_t
         ndarray_fill_index_int16_t 
         ndarray_fill_index_int32_t 
         ndarray_fill_index_int64_t 
         ndarray_fill_index_uint8_t 
         ndarray_fill_index_uint16_t
         ndarray_fill_index_uint32_t
         ndarray_fill_index_uint64_t
         ndarray_sum_float
         ndarray_sum_double
         ndarray_sum_int32_t 
         ndarray_sum_int64_t
         ndarray_sum_uint32_t 
         ndarray_sum_uint64_t
         ndarray_iter_sum_float
         ndarray_iter_sum_double
         ndarray_iter_sum_int32_t 
         ndarray_iter_sum_int64_t
         ndarray_iter_sum_uint32_t 
         ndarray_iter_sum_uint64_t
         ndarray_equal
         ndarray_mul_float
         ndarray_mul_double
         ndarray_mul_int8_t
         ndarray_mul_int16_t 
         ndarray_mul_int32_t 
         ndarray_mul_int64_t 
         ndarray_mul_uint8_t 
         ndarray_mul_uint16_t
         ndarray_mul_uint32_t
         ndarray_mul_uint64_t
         ndarray_add_float
         ndarray_add_double
         ndarray_add_int8_t
         ndarray_add_int16_t 
         ndarray_add_int32_t 
         ndarray_add_int64_t 
         ndarray_add_uint8_t 
         ndarray_add_uint16_t
         ndarray_add_uint32_t
         ndarray_add_uint64_t
         ndarray_sub_float
         ndarray_sub_double
         ndarray_sub_int8_t
         ndarray_sub_int16_t 
         ndarray_sub_int32_t 
         ndarray_sub_int64_t 
         ndarray_sub_uint8_t 
         ndarray_sub_uint16_t
         ndarray_sub_uint32_t
         ndarray_sub_uint64_t
         ndarray_div_float
         ndarray_div_double
         ndarray_div_int8_t
         ndarray_div_int16_t 
         ndarray_div_int32_t 
         ndarray_div_int64_t 
         ndarray_div_uint8_t 
         ndarray_div_uint16_t
         ndarray_div_uint32_t
         ndarray_div_uint64_t
         ndarray_iter_mul_float
         ndarray_iter_mul_double
         ndarray_iter_mul_int8_t
         ndarray_iter_mul_int16_t 
         ndarray_iter_mul_int32_t 
         ndarray_iter_mul_int64_t 
         ndarray_iter_mul_uint8_t 
         ndarray_iter_mul_uint16_t
         ndarray_iter_mul_uint32_t
         ndarray_iter_mul_uint64_t
         ndarray_iter_add_float
         ndarray_iter_add_double
         ndarray_iter_add_int8_t
         ndarray_iter_add_int16_t 
         ndarray_iter_add_int32_t 
         ndarray_iter_add_int64_t 
         ndarray_iter_add_uint8_t 
         ndarray_iter_add_uint16_t
         ndarray_iter_add_uint32_t
         ndarray_iter_add_uint64_t
         ndarray_iter_sub_float
         ndarray_iter_sub_double
         ndarray_iter_sub_int8_t
         ndarray_iter_sub_int16_t 
         ndarray_iter_sub_int32_t 
         ndarray_iter_sub_int64_t 
         ndarray_iter_sub_uint8_t 
         ndarray_iter_sub_uint16_t
         ndarray_iter_sub_uint32_t
         ndarray_iter_sub_uint64_t
         ndarray_iter_div_float
         ndarray_iter_div_double
         ndarray_iter_div_int8_t
         ndarray_iter_div_int16_t 
         ndarray_iter_div_int32_t 
         ndarray_iter_div_int64_t 
         ndarray_iter_div_uint8_t 
         ndarray_iter_div_uint16_t
         ndarray_iter_div_uint32_t
         ndarray_iter_div_uint64_t
         ndarray_expt_float
         ndarray_expt_double
         ndarray_iter_expt_float
         ndarray_iter_expt_double)


(define-ndarray ndarray_fill_float (_fun _NDArray-pointer _float -> _void))
(define-ndarray ndarray_fill_double (_fun _NDArray-pointer _double -> _void))
(define-ndarray ndarray_fill_int8_t (_fun _NDArray-pointer _int8 -> _void))
(define-ndarray ndarray_fill_int16_t (_fun _NDArray-pointer _int16 -> _void))
(define-ndarray ndarray_fill_int32_t (_fun _NDArray-pointer _int32 -> _void))
(define-ndarray ndarray_fill_int64_t (_fun _NDArray-pointer _int64 -> _void))
(define-ndarray ndarray_fill_uint8_t (_fun _NDArray-pointer _uint8 -> _void))
(define-ndarray ndarray_fill_uint16_t (_fun _NDArray-pointer _uint16 -> _void))
(define-ndarray ndarray_fill_uint32_t (_fun _NDArray-pointer _uint32 -> _void))
(define-ndarray ndarray_fill_uint64_t (_fun _NDArray-pointer _uint64 -> _void))

(define-ndarray ndarray_fill_index_float (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_double (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_int8_t (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_int16_t (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_int32_t (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_int64_t (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_uint8_t (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_uint16_t (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_uint32_t (_fun _NDArray-pointer -> _void))
(define-ndarray ndarray_fill_index_uint64_t (_fun _NDArray-pointer -> _void))

(define-ndarray ndarray_sum_float (_fun _NDArray-pointer -> _float))
(define-ndarray ndarray_sum_double (_fun _NDArray-pointer -> _double))
(define-ndarray ndarray_sum_int32_t (_fun _NDArray-pointer -> _int32))
(define-ndarray ndarray_sum_int64_t (_fun _NDArray-pointer -> _int64))
(define-ndarray ndarray_sum_uint32_t (_fun _NDArray-pointer -> _uint32))
(define-ndarray ndarray_sum_uint64_t (_fun _NDArray-pointer -> _uint64))

(define-ndarray ndarray_iter_sum_float (_fun _NDArrayIter-pointer -> _float))
(define-ndarray ndarray_iter_sum_double (_fun _NDArrayIter-pointer -> _double))
(define-ndarray ndarray_iter_sum_int32_t (_fun _NDArrayIter-pointer -> _int32))
(define-ndarray ndarray_iter_sum_int64_t (_fun _NDArrayIter-pointer -> _int64))
(define-ndarray ndarray_iter_sum_uint32_t (_fun _NDArrayIter-pointer -> _uint32))
(define-ndarray ndarray_iter_sum_uint64_t (_fun _NDArrayIter-pointer -> _uint64))

(define-ndarray ndarray_equal (_fun _NDArray-pointer _NDArray-pointer -> _stdbool))

;; Binary operations on NDArrays
;;------------------------------
(define-ndarray ndarray_mul_float (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_float))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_double (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_double))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_int8_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_int8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_int16_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_int16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_int32_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_int32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_int64_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_int64_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_uint8_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_uint16_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_uint16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_uint32_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_uint32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_mul_uint64_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_mul_uint64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_add_float (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_float))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_double (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_double))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_int8_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_int8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_int16_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_int16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_int32_t (_fun _NDArray-pointer _NDArray-pointer
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_int32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_int64_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_int64_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_uint8_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_uint16_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_uint16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_uint32_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_uint32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_add_uint64_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_add_uint64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_sub_float (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_float))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_double (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_double))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_int8_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_int8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_int16_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_int16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_int32_t (_fun _NDArray-pointer _NDArray-pointer
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_int32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_int64_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_int64_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_uint8_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_uint16_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_uint16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_uint32_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_uint32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_sub_uint64_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_sub_uint64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_div_float (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_float))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_double (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_double))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_int8_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_int8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_int16_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_int16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_int32_t (_fun _NDArray-pointer _NDArray-pointer
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_int32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_int64_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_int64_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_uint8_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_uint16_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_uint16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_uint32_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_uint32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_div_uint64_t (_fun _NDArray-pointer _NDArray-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_div_uint64_t))
  #:wrap (allocator ndarray_free))


;; Binary operations on Iterators
;;-------------------------------
(define-ndarray ndarray_iter_mul_float (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_float))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_double (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_double))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_int8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_int8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_int16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_int16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_int32_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_int32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_int64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_int64_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_uint8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_uint16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_uint16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_uint32_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_uint32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_mul_uint64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_mul_uint64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_add_float (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_float))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_double (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_double))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_int8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_int8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_int16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_int16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_int32_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_int32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_int64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_int64_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_uint8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_uint16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_uint16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_uint32_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_uint32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_add_uint64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_add_uint64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_sub_float (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_float))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_double (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_double))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_int8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_int8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_int16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_int16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_int32_t (_fun _NDArray-pointer _NDArray-pointer
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_int32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_int64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_int64_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_uint8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_uint16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_uint16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_uint32_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_uint32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_sub_uint64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_sub_uint64_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_div_float (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_float))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_double (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_double))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_int8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_int8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_int16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_int16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_int32_t (_fun _NDArray-pointer _NDArray-pointer
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_int32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_int64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_int64_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_uint8_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_uint16_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_uint16_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_uint32_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_uint32_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_iter_div_uint64_t (_fun _NDArrayIter-pointer _NDArrayIter-pointer 
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_div_uint64_t))
  #:wrap (allocator ndarray_free))


;; Operations on NDArrays
;;------------------------------
(define-ndarray ndarray_expt_float(_fun _NDArray-pointer _float
                                        -> (p : _NDArray-pointer/null)
                                        -> (check-null p 'ndarray_expt_float))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_expt_double (_fun _NDArray-pointer _double
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_expt_double))
  #:wrap (allocator ndarray_free))


(define-ndarray ndarray_iter_expt_float(_fun _NDArrayIter-pointer _float
                                        -> (p : _NDArray-pointer/null)
                                        -> (check-null p 'ndarray_iter_expt_float))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_expt_double (_fun _NDArrayIter-pointer _double
                                          -> (p : _NDArray-pointer/null)
                                          -> (check-null p 'ndarray_iter_expt_double))
  #:wrap (allocator ndarray_free))


#;(define-ndarray ndarray_mul_double_mp (_fun _NDArray-pointer _NDArray-pointer -> _NDArray-pointer)
  #:wrap (allocator ndarray_free))

