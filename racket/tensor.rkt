#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt")

(provide make-tensor
         tensor-fill
         tref
         t*
         t+
         _double)

(struct tensor
  (type  ; a ctype
   shape ; a vector
   ndarray)
  #:prefab)

(define (guess-type v)
  (cond
    [(double-flonum? v) _double]
    [(single-flonum? v) _float]
    [(exact-integer? v) _int64]
    [else
     _double]))

(define (tensor-fill t v)
  (case (ctype->layout (tensor-type t))
    [(double) (ndarray_fill_double (tensor-ndarray t) v)]
    [(float)  (ndarray_fill_float (tensor-ndarray t))]
    [(int64)  (ndarray_fill_int64_t (tensor-ndarray t))]
    [(uint8)  (ndarray_fill_uint8_t (tensor-ndarray t))]))

(define (make-tensor shape [v 0.0] #:ctype [ctype #f])
  (define type (if ctype
                   ctype
                   (guess-type v)))
  (define nda (ndarray_new (vector-length shape) shape (ctype-sizeof type) #f))
  ;; initialize tensor
  (cond
    [(eq? v 'index)
     (case (ctype->layout type)
       [(double) (ndarray_fill_index_double nda)]
       [(float) (ndarray_fill_index_float nda)]
       [(int64) (ndarray_fill_index_int64_t nda)]
       [(uint8) (ndarray_fill_index_uint8_t nda)]
       [else
        (error "unsupported tensor type")])]
    [else
     (case (ctype->layout type)
       [(double) (ndarray_fill_double nda v)]
       [(float) (ndarray_fill_float nda)]
       [(int64) (ndarray_fill_int64_t nda)]
       [(uint8) (ndarray_fill_uint8_t nda)]
       [else
        (error "unsupported tensor type")])])
  
  (tensor type shape nda))

(define-syntax-rule (tref t i ...)
  (ndarray-ref (tensor-ndarray t) (tensor-type t) i ...))

(define (t* a b)
  (define type (tensor-type a))
  (define shape (tensor-shape a))
  (case (ctype->layout type)
    [(double) (tensor type shape (ndarray_mul_double (tensor-ndarray a) (tensor-ndarray b)))]
    [(float) (tensor type shape (ndarray_mul_float   (tensor-ndarray a) (tensor-ndarray b)))]
    [(int64) (tensor type shape (ndarray_mul_int64_t (tensor-ndarray a) (tensor-ndarray b)))]
    [(uint8) (tensor type shape (ndarray_mul_uint8_t (tensor-ndarray a) (tensor-ndarray b)))]
    [else
     (error "unsupported tensor type")]))

(define (t+ a b)
  (define type (tensor-type a))
  (define shape (tensor-shape a))
  (case (ctype->layout type)
    [(double) (tensor type shape (ndarray_add_double (tensor-ndarray a) (tensor-ndarray b)))]
    [(float) (tensor type shape (ndarray_add_float   (tensor-ndarray a) (tensor-ndarray b)))]
    [(int64) (tensor type shape (ndarray_add_int64_t (tensor-ndarray a) (tensor-ndarray b)))]
    [(uint8) (tensor type shape (ndarray_add_uint8_t (tensor-ndarray a) (tensor-ndarray b)))]
    [else
     (error "unsupported tensor type")]))
