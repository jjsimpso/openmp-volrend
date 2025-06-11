#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt")

(require (for-syntax syntax/parse racket/syntax))

(provide make-tensor
         build-tensor
         tshape
         tlen
         tfill!
         tref
         tslice
         in-tensor
         tmap
         tmap/vector
         t*
         t+
         texpt
         t=?
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

(define (tlen t)
  (if (tensor-iter t)
      (NDArrayIter-length (tensor-iter t))
      (NDArray-num_elems (tensor-ndarray t))))

(define (guess-type v)
  (cond
    [(NDArray? v)
     (case (NDArray-elem_bytes v)
       [(8) _double]
       [(4) _float]
       [(2) _int16]
       [(1) _uint8]
       [else
        (error "Unable to guess tensor's type, supply type with #:ctype")])]
    [(double-flonum? v) _double]
    [(single-flonum? v) _float]
    [(exact-integer? v) _int64]
    [else
     _double]))

(define (guess-default-value type)
  (case (ctype->layout type)
    [(double float) 0.0]
    [else 0]))
    
(define (tfill! t v)
  (case (ctype->layout (tensor-type t))
    [(double) (ndarray_fill_double (tensor-ndarray t) v)]
    [(float)  (ndarray_fill_float (tensor-ndarray t))]
    [(int64)  (ndarray_fill_int64_t (tensor-ndarray t))]
    [(uint8)  (ndarray_fill_uint8_t (tensor-ndarray t))]))

(define (make-tensor shape [val 0.0] #:ctype [ctype #f])
  (define type (if ctype
                   ctype
                   (guess-type val)))
  ;; pick default value if not explicitly set and type isn't double
  (define v (if (and (equal? val 0.0) (not (eq? type 'double)))
                (guess-default-value type)
                val))
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
       [(float) (ndarray_fill_float nda v)]
       [(int64) (ndarray_fill_int64_t nda v)]
       [(uint8) (ndarray_fill_uint8_t nda v)]
       [else
        (error "unsupported tensor type")])])
  
  (tensor type shape nda))

(define (build-tensor shape proc ctype)
  (define t (make-tensor shape #:ctype ctype))
  (define dataptr (NDArray-dataptr (tensor-ndarray t)))
  (for ([v (in-tensor t)]
        [i (in-naturals 0)])
    (ptr-set! dataptr ctype i (proc i)))
  t)

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

(define (in-tensor/proc t)
  (for/vector ([v (in-tensor t)])
    v))

(define-sequence-syntax in-tensor
  (lambda () #'in-tensor/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(val) (_ expr)]
       #'[(val)
          (:do-in
           ([(it) (if (tensor-iter expr)
                      (tensor-iter expr)
                      (ndarray_iter_new (tensor-ndarray expr) #f))]
            [(type) (tensor-type expr)])
           (unless (NDArrayIter? it)
             (raise-argument-error 'in-iter "NDArrayiter?" it))
           ([n it])
           #t
           ([(val) (ndarray-iter-data n type)])
           #t
           ;; advance cursor in iterator and continue or reset iterator and quit
           (if (ndarray_iter_next n)
               #t
               (begin
                 (ndarray_iter_reset n)
                 #f))
           [n])]]
      [_ #false])))

;; use tmap when the tensor operation you need doesn't exist
(define tmap
  (case-lambda
    [(proc t)
     (define type (tensor-type t))
     (define result (make-tensor (tshape t) #:ctype type))
     (define result-dataptr (NDArray-dataptr (tensor-ndarray result)))
     (for ([x (in-tensor t)]
           [i (in-naturals 0)])
       (ptr-set! result-dataptr type i (proc x)))
     result]
    [(proc ta tb)
     (define type (tensor-type ta))
     (unless (and (eq? type (tensor-type tb))
                  (equal? (tensor-shape ta) (tensor-shape tb)))
       (error "tensor arguments are incompatible in either shape or type"))
     (define result (make-tensor (tshape ta) #:ctype type))
     (define result-dataptr (NDArray-dataptr (tensor-ndarray result)))
     (for ([x (in-tensor ta)]
           [y (in-tensor tb)]
           [i (in-naturals 0)])
       (ptr-set! result-dataptr type i (proc x y)))
     result]))

;; like tmap but returns a vector instead of a tensor
;; useful for interacting with other racket code
(define tmap/vector
  (case-lambda
    [(proc t)
     (for/vector #:length (tlen t)
                 ([x (in-tensor t)])
       (proc x))]
    [(proc ta tb)
     (define type (tensor-type ta))
     (unless (and (eq? type (tensor-type tb))
                  (equal? (tensor-shape ta) (tensor-shape tb)))
       (error "tensor arguments are incompatible in either shape or type"))
     (for/vector #:length (tlen ta)
                 ([x (in-tensor ta)]
                  [y (in-tensor tb)])
       (proc x y))]))

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
        [(int8)  ndarray_iter_mul_int8_t]
        [(int16) ndarray_iter_mul_int16_t]
        [(int32) ndarray_iter_mul_int32_t]
        [(int64) ndarray_iter_mul_int64_t]
        [(uint8)  ndarray_iter_mul_uint8_t]
        [(uint16) ndarray_iter_mul_uint16_t]
        [(uint32) ndarray_iter_mul_uint32_t]
        [(uint64) ndarray_iter_mul_uint64_t]
        [else
         (error "unsupported tensor type")])
      (case type
        [(double) ndarray_mul_double]
        [(float) ndarray_mul_float]
        [(int8)  ndarray_mul_int8_t]
        [(int16) ndarray_mul_int16_t]
        [(int32) ndarray_mul_int32_t]
        [(int64) ndarray_mul_int64_t]
        [(uint8)  ndarray_mul_uint8_t]
        [(uint16) ndarray_mul_uint16_t]
        [(uint32) ndarray_mul_uint32_t]
        [(uint64) ndarray_mul_uint64_t]
        [else
         (error "unsupported tensor type")])))

(define (dispatch-div type iter?)
  (if iter?
      (case type
        [(double) ndarray_iter_div_double]
        [(float) ndarray_iter_div_float]
        [(int8)  ndarray_iter_div_int8_t]
        [(int16) ndarray_iter_div_int16_t]
        [(int32) ndarray_iter_div_int32_t]
        [(int64) ndarray_iter_div_int64_t]
        [(uint8)  ndarray_iter_div_uint8_t]
        [(uint16) ndarray_iter_div_uint16_t]
        [(uint32) ndarray_iter_div_uint32_t]
        [(uint64) ndarray_iter_div_uint64_t]
        [else
         (error "unsupported tensor type")])
      (case type
        [(double) ndarray_div_double]
        [(float) ndarray_div_float]
        [(int8)  ndarray_div_int8_t]
        [(int16) ndarray_div_int16_t]
        [(int32) ndarray_div_int32_t]
        [(int64) ndarray_div_int64_t]
        [(uint8)  ndarray_div_uint8_t]
        [(uint16) ndarray_div_uint16_t]
        [(uint32) ndarray_div_uint32_t]
        [(uint64) ndarray_div_uint64_t]
        [else
         (error "unsupported tensor type")])))

(define (dispatch-add type iter?)
  (if iter?
      (case type
        [(double) ndarray_iter_add_double]
        [(float) ndarray_iter_add_float]
        [(int8)  ndarray_iter_add_int8_t]
        [(int16) ndarray_iter_add_int16_t]
        [(int32) ndarray_iter_add_int32_t]
        [(int64) ndarray_iter_add_int64_t]
        [(uint8)  ndarray_iter_add_uint8_t]
        [(uint16) ndarray_iter_add_uint16_t]
        [(uint32) ndarray_iter_add_uint32_t]
        [(uint64) ndarray_iter_add_uint64_t]
        [else
         (error "unsupported tensor type")])
      (case type
        [(double) ndarray_add_double]
        [(float) ndarray_add_float]
        [(int8)  ndarray_add_int8_t]
        [(int16) ndarray_add_int16_t]
        [(int32) ndarray_add_int32_t]        
        [(int64) ndarray_add_int64_t]
        [(uint8)  ndarray_add_uint8_t]
        [(uint16) ndarray_add_uint16_t]
        [(uint32) ndarray_add_uint32_t]
        [(uint64) ndarray_add_uint64_t]
        [else
         (error "unsupported tensor type")])))

(define (dispatch-sub type iter?)
  (if iter?
      (case type
        [(double) ndarray_iter_sub_double]
        [(float) ndarray_iter_sub_float]
        [(int8)  ndarray_iter_sub_int8_t]
        [(int16) ndarray_iter_sub_int16_t]
        [(int32) ndarray_iter_sub_int32_t]
        [(int64) ndarray_iter_sub_int64_t]
        [(uint8)  ndarray_iter_sub_uint8_t]
        [(uint16) ndarray_iter_sub_uint16_t]
        [(uint32) ndarray_iter_sub_uint32_t]
        [(uint64) ndarray_iter_sub_uint64_t]
        [else
         (error "unsupported tensor type")])
      (case type
        [(double) ndarray_sub_double]
        [(float) ndarray_sub_float]
        [(int8)  ndarray_sub_int8_t]
        [(int16) ndarray_sub_int16_t]
        [(int32) ndarray_sub_int32_t]        
        [(int64) ndarray_sub_int64_t]
        [(uint8)  ndarray_sub_uint8_t]
        [(uint16) ndarray_sub_uint16_t]
        [(uint32) ndarray_sub_uint32_t]
        [(uint64) ndarray_sub_uint64_t]
        [else
         (error "unsupported tensor type")])))

(define (dispatch-equal type)
  (case type
    [(double) ndarray_iter_equal_double]
    [(float) ndarray_iter_equal_float]
    [(int8)  ndarray_iter_equal_int8_t]
    [(int16) ndarray_iter_equal_int16_t]
    [(int32) ndarray_iter_equal_int32_t]
    [(int64) ndarray_iter_equal_int64_t]
    [(uint8)  ndarray_iter_equal_uint8_t]
    [(uint16) ndarray_iter_equal_uint16_t]
    [(uint32) ndarray_iter_equal_uint32_t]
    [(uint64) ndarray_iter_equal_uint64_t]
    [else
     (error "unsupported tensor type")]))

(define (t* a b)
  (define type (tensor-type a))
  (cond
    [(and (false? (tensor-iter a)) (false? (tensor-iter b)))
     (define result ((dispatch-mul (ctype->layout type) #f) (tensor-ndarray a) (tensor-ndarray b)))
     (tensor type (ndarray-dims->shape result) result)]
    [(and (tensor-iter a) (tensor-iter b))
     (define result ((dispatch-mul (ctype->layout type) #t) (tensor-iter a) (tensor-iter b)))
     (tensor type (ndarray-dims->shape result) result)]
    [(tensor-iter a)
     (define result ((dispatch-mul (ctype->layout type) #t) (tensor-iter a) (ndarray_iter_new (tensor-ndarray b) #f)))
     (tensor type (ndarray-dims->shape result) result)]
    [(tensor-iter b)
     (define result ((dispatch-mul (ctype->layout type) #t) (ndarray_iter_new (tensor-ndarray a) #f) (tensor-iter b)))
     (tensor type (ndarray-dims->shape result) result)]
    [else
     (error "incompatible tensor arguments")]))

(define (t+ a b)
  (define type (tensor-type a))
  (cond
    [(and (false? (tensor-iter a)) (false? (tensor-iter b)))
     (define result ((dispatch-add (ctype->layout type) #f) (tensor-ndarray a) (tensor-ndarray b)))
     (tensor type (ndarray-dims->shape result) result)]
    [(and (tensor-iter a) (tensor-iter b))
     (define result ((dispatch-add (ctype->layout type) #t) (tensor-iter a) (tensor-iter b)))
     (tensor type (ndarray-dims->shape result) result)]
    [else
     (error "incompatible tensor arguments")]))

(define (t=? a b)
  (cond
    [(and (false? (tensor-iter a)) (false? (tensor-iter b)))
     (ndarray_equal (tensor-ndarray a) (tensor-ndarray b))]
    [(and (tensor-iter a) (tensor-iter b))
     ((dispatch-equal (ctype->layout (tensor-type a))) (tensor-iter a) (tensor-iter b))]
    [(tensor-iter a)
     ((dispatch-equal (ctype->layout (tensor-type a))) (tensor-iter a) (ndarray_iter_new (tensor-ndarray b) #f))]
    [(tensor-iter b)
     ((dispatch-equal (ctype->layout (tensor-type a))) (ndarray_iter_new (tensor-ndarray a) #f) (tensor-iter b))]
    [else
     (error "incompatible tensor arguments")]))

(define (texpt t w)
  (define type (tensor-type t))
  (define shape (tshape t))
  (if (tensor-iter t)
      (case (ctype->layout type)
        [(double) (tensor type shape (ndarray_iter_expt_double (tensor-iter t) w))]
        [(float) (tensor type shape (ndarray_iter_expt_float (tensor-iter t) w))]
        [else
         (error "unsupported tensor type" type)])
      (case (ctype->layout type)
        [(double) (tensor type shape (ndarray_expt_double (tensor-ndarray t) w))]
        [(float) (tensor type shape (ndarray_expt_float (tensor-ndarray t) w))]
        [else
         (error "unsupported tensor type" type)])))
