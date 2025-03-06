#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         ffi/unsafe/cvector)

(provide define-ndarray
         _intptr-pointer
         _ndarray-pointer
         _ndarray_iter-pointer
         _slice-pointer
         ndarray_new
         ndarray_free
         ndarray_iter_new
         ndarray_iter_new_all_but_axis
         ndarray_iter_free
         ndarray-ref
         ndarray-dims
         _NDArray
         (struct-out NDArray))

(define-ffi-definer define-ndarray (ffi-lib "../libvolrend"))

(define MAX-DIMS 32)

(define _intptr-pointer (_cpointer 'intptr_t))

(define _ndarray-pointer (_cpointer 'NDArray))
(define _ndarray_iter-pointer (_cpointer 'NDArray))
(define _slice-pointer (_cpointer 'Slice))

(define _intptr-array-max-dims (_array _intptr MAX-DIMS))

(define-cstruct _NDArray
  ([ndim _int]
   [dims _intptr-pointer]
   [num_elems _intptr]
   [elem_bytes _intptr]
   [size _intptr]
   [free_data _bool]
   [dataptr _pointer]))

#;(define-cstruct _NDArrayIter
  ([nd_m1 _int]
   [index _intptr]
   [length _intptr]
   [coords _intptr-array-max-dims] ;; (make-array-type _intptr MAX-DIMS)
   [dims_m1 _intptr-array-max-dims] 
   [strides _intptr-array-max-dims]
   [backstrides _intptr-array-max-dims]
   [slicestarts _intptr-array-max-dims]
   [nda _ndarray-pointer]
   [cursor __pointer]
   [contiguous _bool]))

(define-cstruct _Slice
  ([start _intptr]
   [end _intptr]
   [stride _intptr]))

(define (check-null p who)
  (if (false? p)
      (error who "returned NULL")
      p))

(define-ndarray ndarray_free (_fun _ndarray-pointer -> _void)
  #:wrap (deallocator))
(define-ndarray ndarray_new (_fun _int [dims : (_vector i _intptr)] _intptr _pointer
                                  -> (p : _ndarray-pointer)
                                  -> (check-null p 'ndarray_new))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_iter_free (_fun _ndarray_iter-pointer -> _void)
  #:wrap (deallocator))
(define-ndarray ndarray_iter_new (_fun _ndarray-pointer _slice-pointer
                                       -> (p : _ndarray_iter-pointer)
                                       -> (check-null p 'ndarray_iter_new))
  #:wrap (allocator ndarray_iter_free))
(define-ndarray ndarray_iter_new_all_but_axis (_fun _ndarray-pointer _slice-pointer (_cpointer 'int)
                                                    -> (p : _ndarray_iter-pointer)
                                                    -> (check-null p 'ndarray_new))
  #:wrap (allocator ndarray_iter_free))

#;(define (ndarray-ref p type i)
  (define nda (ptr-ref p _NDArray))
  (ptr-ref (NDArray-dataptr nda) type i))

(define-syntax-rule (ndarray-ref p type i ...)
  (ptr-ref (NDArray-dataptr (ptr-ref p _NDArray)) type i ...))

(define (ndarray-dims p i)
  (ptr-ref (NDArray-dims (ptr-ref p _NDArray)) _intptr i))
