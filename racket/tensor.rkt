#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt")

(provide make-tensor
         tshape
         tfill!
         tref
         tslice
         t*
         t+
         (struct-out tensor)
         NDArray?
         _double)

(struct tensor
  (type  ; a ctype
   shape ; a vector
   ndarray
   [iter #:auto #:mutable])
  #:auto-value #f
  #:prefab)

(define (iter-shape it)
  (for/vector #:length (add1 (NDArrayIter-nd_m1 it))
              ([i (in-naturals)])
    (ndarray-iter-dims it i)))

(define (tshape t)
  (if (tensor-iter t)
      (iter-shape (tensor-iter t))
      (tensor-shape t)))

(define (guess-type v)
  (cond
    [(NDArray? v)
     (case (NDArray-elem_bytes v)
       [(8) _double]
       [(4) _float]
       [(1) _uint8]
       [else _double])]
    [(double-flonum? v) _double]
    [(single-flonum? v) _float]
    [(exact-integer? v) _int64]
    [else
     _double]))

(define (tfill! t v)
  (case (ctype->layout (tensor-type t))
    [(double) (ndarray_fill_double (tensor-ndarray t) v)]
    [(float)  (ndarray_fill_float (tensor-ndarray t))]
    [(int64)  (ndarray_fill_int64_t (tensor-ndarray t))]
    [(uint8)  (ndarray_fill_uint8_t (tensor-ndarray t))]))

(define (make-tensor shape [v 0.0] #:ctype [ctype #f])
  (define type (if ctype
                   ctype
                   (guess-type v)))
  (define nda #f)
  
  ;; initialize tensor
  (cond
    [(NDArray? v)
     ;; todo: add shape check
     (set! nda v)]
    [(eq? v 'index)
     (set! nda (ndarray_new (vector-length shape) shape (ctype-sizeof type) #f))
     (case (ctype->layout type)
       [(double) (ndarray_fill_index_double nda)]
       [(float) (ndarray_fill_index_float nda)]
       [(int64) (ndarray_fill_index_int64_t nda)]
       [(uint8) (ndarray_fill_index_uint8_t nda)]
       [else
        (error "unsupported tensor type")])]
    [else
     (set! nda (ndarray_new (vector-length shape) shape (ctype-sizeof type) #f))
     (case (ctype->layout type)
       [(double) (ndarray_fill_double nda v)]
       [(float) (ndarray_fill_float nda)]
       [(int64) (ndarray_fill_int64_t nda)]
       [(uint8) (ndarray_fill_uint8_t nda)]
       [else
        (error "unsupported tensor type")])])
  
  (tensor type shape nda))

;; returns a new tensor that points to the same ndarray
(define (tslice t slice-list #:skip-dim [skip-dim #f])
  (define tnew (tensor (tensor-type t) (tensor-shape t) (tensor-ndarray t)))
  (define nd (if skip-dim
                 (sub1 (vector-length (tensor-shape tnew)))
                 (vector-length (tensor-shape tnew))))
  
  (unless (or (empty? slice-list)
              (equal? nd (length slice-list)))
    (error "slices don't match tensor dimensions"))
  (define slices (if (empty? slice-list)
                     #f
                     (make-slice nd slice-list)))
  (define it
    (if skip-dim
        (let ([dim (malloc _int)])
          (ptr-set! dim _int 0 skip-dim)
          (ndarray_iter_new_all_but_axis (tensor-ndarray t) slices dim))
        (ndarray_iter_new (tensor-ndarray t) slices)))
  
  (set-tensor-iter! tnew it)
  tnew)

(define-syntax-rule (tref t i ...)
  (ndarray-ref (tensor-ndarray t) (tensor-type t) i ...))

;(require (for-syntax syntax/parse racket/syntax))

#;(define-syntax (op-name stx)
  (syntax-parse stx
    #:datum-literals (iterator)
    [(_ op type)
     (with-syntax ([name (format-id stx "ndarray_~a_~a" (syntax-e #'op) (syntax-e #'type))])
       #'name)]
    [(_ op type iterator)
     (with-syntax ([name (format-id stx "ndarray_iter_~a_~a" (syntax-e #'op) (syntax-e #'type))])
       #'name)]
    #;[(_) #'"dispatch-op: invalid syntax"]))

(define (dispatch-mul type iter?)
  (if iter?
      (case type
        [(double) ndarray_iter_mul_double]
        [(float) ndarray_iter_mul_float]
        [(int64) ndarray_iter_mul_int64_t]
        [(uint8) ndarray_iter_mul_uint8_t]
        [else
         (error "unsupported tensor type")])
      (case type
        [(double) ndarray_mul_double]
        [(float) ndarray_mul_float]
        [(int64) ndarray_mul_int64_t]
        [(uint8) ndarray_mul_uint8_t]
        [else
         (error "unsupported tensor type")])))

(define (dispatch-add type iter?)
  (if iter?
      (case type
        [(double) ndarray_iter_add_double]
        [(float) ndarray_iter_add_float]
        [(int64) ndarray_iter_add_int64_t]
        [(uint8) ndarray_iter_add_uint8_t]
        [else
         (error "unsupported tensor type")])
      (case type
        [(double) ndarray_add_double]
        [(float) ndarray_add_float]
        [(int64) ndarray_add_int64_t]
        [(uint8) ndarray_add_uint8_t]
        [else
         (error "unsupported tensor type")])))

(define (t* a b)
  (define type (tensor-type a))
  (define shape (tshape a))
  (cond
    [(and (false? (tensor-iter a)) (false? (tensor-iter b)))
     (tensor type shape ((dispatch-mul (ctype->layout type) #f) (tensor-ndarray a) (tensor-ndarray b)))]
    [(and (tensor-iter a) (tensor-iter b))
     (tensor type shape ((dispatch-mul (ctype->layout type) #t) (tensor-iter a) (tensor-iter b)))]
    [else
     (error "incompatible tensor arguments")]))

(define (t+ a b)
  (define type (tensor-type a))
  (define shape (tshape a))
  (cond
    [(and (false? (tensor-iter a)) (false? (tensor-iter b)))
     (tensor type shape ((dispatch-add (ctype->layout type) #f) (tensor-ndarray a) (tensor-ndarray b)))]
    [(and (tensor-iter a) (tensor-iter b))
     (tensor type shape ((dispatch-add (ctype->layout type) #t) (tensor-iter a) (tensor-iter b)))]
    [else
     (error "incompatible tensor arguments")]))
