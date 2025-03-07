#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         ffi/unsafe/cvector)

(provide define-ndarray
         _intptr-pointer
         ndarray_new
         ndarray_free
         ndarray_iter_new
         ndarray_iter_new_all_but_axis
         ndarray_iter_free
         ndarray_iter_next
         ndarray_iter_reset
         ndarray-ref
         ndarray-dims
         _NDArray-pointer
         _NDArrayIter-pointer
         _Slice-pointer
         (struct-out NDArray)
         (struct-out NDArrayIter)
         (struct-out Slice))

(define-ffi-definer define-ndarray (ffi-lib "../libvolrend"))

(define MAX-DIMS 32)

(define _intptr-pointer (_cpointer 'intptr_t))

(define _intptr-array-max-dims (_array _intptr MAX-DIMS))

(define-cstruct _NDArray
  ([ndim _int]
   [dims _intptr-pointer]
   [num_elems _intptr]
   [elem_bytes _intptr]
   [size _intptr]
   [free_data _bool]
   [dataptr _pointer]))

(define-cstruct _NDArrayIter
  ([nd_m1 _int]
   [index _intptr]
   [length _intptr]
   [coords _intptr-array-max-dims] ;; (make-array-type _intptr MAX-DIMS)
   [dims_m1 _intptr-array-max-dims] 
   [strides _intptr-array-max-dims]
   [backstrides _intptr-array-max-dims]
   [slicestarts _intptr-array-max-dims]
   [nda _NDArray-pointer]
   [cursor _pointer]
   [contiguous _bool]))

(define-cstruct _Slice
  ([start _intptr]
   [end _intptr]
   [stride _intptr]))

(define (check-null p who)
  (if (false? p)
      (error who "returned NULL")
      p))

(define-ndarray ndarray_free (_fun _NDArray-pointer -> _void)
  #:wrap (deallocator))
(define-ndarray ndarray_new (_fun _int [dims : (_vector i _intptr)] _intptr _pointer
                                  -> (p : _NDArray-pointer)
                                  -> (check-null p 'ndarray_new))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_free (_fun _NDArrayIter-pointer -> _void)
  #:wrap (deallocator))
(define-ndarray ndarray_iter_new (_fun _NDArray-pointer _Slice-pointer/null
                                       -> (p : _NDArrayIter-pointer)
                                       -> (check-null p 'ndarray_iter_new))
  #:wrap (allocator ndarray_iter_free))
(define-ndarray ndarray_iter_new_all_but_axis (_fun _NDArray-pointer _Slice-pointer/null (_cpointer 'int)
                                                    -> (p : _NDArrayIter-pointer)
                                                    -> (check-null p 'ndarray_new))
  #:wrap (allocator ndarray_iter_free))

(define-ndarray ndarray_iter_next (_fun _NDArrayIter-pointer -> _bool))
(define-ndarray ndarray_iter_reset (_fun _NDArrayIter-pointer -> _void))


#;(define (ndarray-ref p type i)
  (define nda (ptr-ref p _NDArray))
  (ptr-ref (NDArray-dataptr nda) type i))

(define-syntax-rule (ndarray-ref p type i ...)
  (ptr-ref (NDArray-dataptr p) type i ...))

(define (ndarray-dims p i)
  (ptr-ref (NDArray-dims (ptr-ref p _NDArray)) _intptr i))
