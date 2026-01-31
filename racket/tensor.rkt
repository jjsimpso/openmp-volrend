#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt"
         "ndarray-matrix-ffi.rkt")

(require (for-syntax syntax/parse racket/syntax))

(provide make-tensor
         build-tensor
         print-tensor
         tshape
         tlen
         tfill!
         tref
         tset!
         tslice
         in-tensor
         tmap
         tmap/vector
         t*
         t+
         t/
         t-
         t**
         tsqrt
         texpt
         t=?
         tsum
         (struct-out tensor)
         NDArray?
         _double
         _complex)

(struct tensor
  (type  ; a ctype
   shape ; a vector
   ndarray
   [iter #:auto #:mutable])
  #:auto-value #f
  #:prefab)

(define (print-tensor t)
  (define (insert-padding n)
    (for ([i (in-range 0 n)])
      (display " ")))
  (define (print-axis it type shape-vec depth)
    (cond
      [(= (vector-length shape-vec) 1)
       (printf "[")
       (for ([i (in-range 0 (sub1 (vector-ref shape-vec 0)))])
         (printf "~a, " (ndarray-iter-data-advance it type)))
       (printf "~a]" (ndarray-iter-data-advance it type))]
      [(= (vector-length shape-vec) 2)
       (define row-shape (vector-drop shape-vec 1))
       (printf "[")
       (for ([i (in-range 0 (sub1 (vector-ref shape-vec 0)))])
         (print-axis it type row-shape (add1 depth))
         (printf ",~n")
         (insert-padding depth))
       (print-axis it type row-shape (add1 depth))
       (printf "]")]
      [else
       (define sub-shape (vector-drop shape-vec 1))
       (printf "[")
       (for ([i (in-range 0 (sub1 (vector-ref shape-vec 0)))])
         (print-axis it type sub-shape (add1 depth))
         (printf ",~n~n")
         (insert-padding depth))
       (print-axis it type sub-shape (add1 depth))
       (printf "]")]))

  (cond
    [(tensor-iter t)
     (ndarray_iter_reset (tensor-iter t))
     (print-axis (tensor-iter t) (tensor-type t) (tshape t) 1)
     (printf "~n")]
    [else
     (print-axis (ndarray_iter_new (tensor-ndarray t) #f) (tensor-type t) (tshape t) 1)
     (printf "~n")]))

(define (iter-shape it)
  (for/vector #:length (add1 (NDArrayIter-nd_m1 it))
              ([i (in-naturals)])
    (ndarray-iter-dims it i)))

(define (tshape t)
  (tensor-shape t)
  #;(if (tensor-iter t)
      (iter-shape (tensor-iter t))
      (tensor-shape t)))

(define (tlen t [start-dim #f])
  (if (false? start-dim)
      (if (tensor-iter t)
          (NDArrayIter-length (tensor-iter t))
          (NDArray-num_elems (tensor-ndarray t)))
      (if (tensor-iter t)
          (ndarray-iter-sub-elems (tensor-iter t) start-dim)
          (ndarray-sub-elems (tensor-ndarray t) start-dim))))

;; gets a tensor's iterator or creates a new iterator for it
(define (tensor-as-iter t)
  (or (tensor-iter t)
      (ndarray_iter_new (tensor-ndarray t) #f)))

(define (number->ndarray n type)
  (define scalar-ptr (malloc type 'raw))
  (case (ctype->layout type)
    [(double float) (ptr-set! scalar-ptr type 0 (exact->inexact n))]
    ['(double double) (ptr-set! scalar-ptr type 0 (exact->inexact n))]
    [(int8 int16 int32 int64) (ptr-set! scalar-ptr type 0 (exact-truncate n))]
    [(uint8 uint16 uint32 uint64) (ptr-set! scalar-ptr type 0 (exact-truncate n))]
    [else (error "Unsupported scalar type conversion to tensor")])
  (define nda (ndarray_new 1 (vector 1) (ctype-sizeof type) scalar-ptr))
  (ndarray_set_freedata nda #t)
  nda)

(define (tensor-ndarray-or-iter t type iter?)
  (cond
    [(number? t)
     ; create an iterator from the number t
     (ndarray_iter_new (number->ndarray t type) #f)]
    [iter?
     (tensor-as-iter t)]
    [else
     (tensor-ndarray t)]))

(define (guess-type v)
  (cond
    [(NDArray? v)
     (case (NDArray-elem_bytes v)
       [(16) _complex]
       [(8) _double]
       [(4) _float]
       [(2) _int16]
       [(1) _uint8]
       [else
        (error "Unable to guess tensor's type, supply type with #:ctype")])]
    [(vector? v)
     (define val (vector-ref v 0))
     (cond
       [(double-flonum? val) _double]
       [(single-flonum? val) _float]
       [(exact-integer? val) _int64]
       [else
        _double])]
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
    [(float)  (ndarray_fill_float (tensor-ndarray t) v)]
    ['(double double) (ndarray_fill_complex (tensor-ndarray t) v)]
    [(int8)   (ndarray_fill_int8_t (tensor-ndarray t) v)]
    [(int16)  (ndarray_fill_int16_t (tensor-ndarray t) v)]
    [(int32)  (ndarray_fill_int32_t (tensor-ndarray t) v)]
    [(int64)  (ndarray_fill_int64_t (tensor-ndarray t) v)]
    [(uint8)  (ndarray_fill_uint8_t (tensor-ndarray t) v)]
    [(uint16) (ndarray_fill_uint16_t (tensor-ndarray t) v)]
    [(uint32) (ndarray_fill_uint32_t (tensor-ndarray t) v)]
    [(uint64) (ndarray_fill_uint64_t (tensor-ndarray t) v)]))

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
    [(vector? v)
     (set! nda (ndarray_new (vector-length shape) shape (ctype-sizeof type) #f))
     (for ([i (in-range 0 (NDArray-num_elems nda))])
       (ndarray-set! nda type i (vector-ref v i)))]
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
       ['(double double) (ndarray_fill_complex nda v)]
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

;; replace any empty slices with values representing the full slice before calling make-slice
;; (length slice-list) must equal nd
(define (preprocess-slice-list t slice-list skip-dim)
  (define shape (tensor-shape t))
  (let loop ([i 0]
             [old-list slice-list]
             [new-list '()])
    (cond
      [(empty? old-list)
       (reverse new-list)]
      [(equal? i skip-dim)
       (loop (add1 i) old-list new-list)]
      [(empty? (car old-list))
       (loop (add1 i)
             (cdr old-list)
             (cons `(0 ,(sub1 (vector-ref shape i)) 1) new-list))]
      [else
       (loop (add1 i)
             (cdr old-list)
             (cons (car old-list) new-list))])))
      
#|       
  (for/list ([i (in-naturals 0)]
             #:unless (equal? i skip-dim)
             [slice (in-list slice-list)])
    (if (empty? slice)
        `(0 ,(sub1 (vector-ref shape i)) 1)
        slice)))
|#

(define (vector-remove-at v i)
  (define-values (head tail) (vector-split-at v i))
  (vector-append head (vector-drop tail 1)))

(define (vector-insert-at v i)
  (define-values (head tail) (vector-split-at v i))
  (vector-append head (vector 1) tail))

;; returns a new tensor that points to the same ndarray
(define (tslice t slice-list #:skip-dim [skip-dim #f] #:add-dim [add-dim #f])
  (and skip-dim add-dim (error "skip-dim and add-dim keywords are mutually exclusize"))
  (define tnew (tensor (tensor-type t)
                       (cond
                         [skip-dim
                          (if (= skip-dim -1)
                              (vector-drop-right (tensor-shape t) 1)
                              (vector-remove-at (tensor-shape t) skip-dim))]
                         [add-dim
                          (vector-insert-at (tensor-shape t) add-dim)]
                         [else
                          (tensor-shape t)])
                       (tensor-ndarray t)))
  (define nd (vector-length (tensor-shape tnew)))
  
  (unless (or (empty? slice-list)
              (equal? nd (length slice-list)))
    (error "slices don't match tensor dimensions"))
  (define slices (if (empty? slice-list)
                     #f
                     (make-slice nd (preprocess-slice-list t slice-list skip-dim))))
  (define it
    (cond
      [skip-dim
       (let ([dim (malloc _int)])
         (ptr-set! dim _int 0 skip-dim)
         (ndarray_iter_new_all_but_axis (tensor-ndarray t) slices dim))]
      [add-dim
       (ndarray_iter_new_add_axis (tensor-ndarray t) slices add-dim)]
      [else
       (ndarray_iter_new (tensor-ndarray t) slices)]))

  ;; modify shape vector with new sizes
  (for/fold ([shape-idx 0])
            ([it-idx (in-range 0 (add1 (NDArrayIter-nd_m1 it)))])
    (define dim-size (ndarray-iter-dims it it-idx))
    (if (equal? it-idx skip-dim)
        shape-idx
        (begin
          (vector-set! (tensor-shape tnew) shape-idx dim-size)
          (add1 shape-idx))))
  
  (set-tensor-iter! tnew it)
  tnew)

(define-syntax-rule (tref t i ...)
  (cond
    [(> (* (add1 i) ...) (tlen t))
     (error "tref index out of bounds")]
    [(tensor-iter t)
     (error "tref doesn't support iterators")]
    [else
      (ndarray-ref (tensor-ndarray t) (tensor-type t) i ...)]))

(define-syntax-rule (tset! t i ... v)
  (cond
    [(> (* (add1 i) ...) (tlen t))
     (error "tset! index out of bounds")]
    [(tensor-iter t)
     (error "tref doesn't support iterators")]
    [else
     (ndarray-set! (tensor-ndarray t) (tensor-type t) i ... v)]))

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

(define-syntax (define-op-dispatch stx)
  (syntax-parse stx
    [(_ op)
     (with-syntax ([name (format-id stx "dispatch-~a" (syntax-e #'op))]
                   [iter_op_double (format-id stx "ndarray_iter_~a_double" (syntax-e #'op))]
                   [iter_op_float (format-id stx "ndarray_iter_~a_float" (syntax-e #'op))]
                   [iter_op_complex (format-id stx "ndarray_iter_~a_complex" (syntax-e #'op))]
                   [iter_op_int8_t (format-id stx "ndarray_iter_~a_int8_t" (syntax-e #'op))]
                   [iter_op_int16_t (format-id stx "ndarray_iter_~a_int16_t" (syntax-e #'op))]
                   [iter_op_int32_t (format-id stx "ndarray_iter_~a_int32_t" (syntax-e #'op))]
                   [iter_op_int64_t (format-id stx "ndarray_iter_~a_int64_t" (syntax-e #'op))]
                   [iter_op_uint8_t (format-id stx "ndarray_iter_~a_uint8_t" (syntax-e #'op))]
                   [iter_op_uint16_t (format-id stx "ndarray_iter_~a_uint16_t" (syntax-e #'op))]
                   [iter_op_uint32_t (format-id stx "ndarray_iter_~a_uint32_t" (syntax-e #'op))]
                   [iter_op_uint64_t (format-id stx "ndarray_iter_~a_uint64_t" (syntax-e #'op))]
                   [op_double (format-id stx "ndarray_~a_double" (syntax-e #'op))]
                   [op_float (format-id stx "ndarray_~a_float" (syntax-e #'op))]
                   [op_complex (format-id stx "ndarray_~a_complex" (syntax-e #'op))]
                   [op_int8_t (format-id stx "ndarray_~a_int8_t" (syntax-e #'op))]
                   [op_int16_t (format-id stx "ndarray_~a_int16_t" (syntax-e #'op))]
                   [op_int32_t (format-id stx "ndarray_~a_int32_t" (syntax-e #'op))]
                   [op_int64_t (format-id stx "ndarray_~a_int64_t" (syntax-e #'op))]
                   [op_uint8_t (format-id stx "ndarray_~a_uint8_t" (syntax-e #'op))]
                   [op_uint16_t (format-id stx "ndarray_~a_uint16_t" (syntax-e #'op))]
                   [op_uint32_t (format-id stx "ndarray_~a_uint32_t" (syntax-e #'op))]
                   [op_uint64_t (format-id stx "ndarray_~a_uint64_t" (syntax-e #'op))])
       #'(define (name type iter?)
           (if iter?
               (case (ctype->layout type)
                 [(double) iter_op_double]
                 [(float) iter_op_float]
                 ['(double double) iter_op_complex]
                 [(int8)  iter_op_int8_t]
                 [(int16) iter_op_int16_t]
                 [(int32) iter_op_int32_t]
                 [(int64) iter_op_int64_t]
                 [(uint8)  iter_op_uint8_t]
                 [(uint16) iter_op_uint16_t]
                 [(uint32) iter_op_uint32_t]
                 [(uint64) iter_op_uint64_t]
                 [else
                  (error "unsupported tensor type")])
               (case (ctype->layout type)
                 [(double) op_double]
                 [(float) op_float]
                 ['(double double) op_complex]
                 [(int8)  op_int8_t]
                 [(int16) op_int16_t]
                 [(int32) op_int32_t]
                 [(int64) op_int64_t]
                 [(uint8)  op_uint8_t]
                 [(uint16) op_uint16_t]
                 [(uint32) op_uint32_t]
                 [(uint64) op_uint64_t]
                 [else
                  (error "unsupported tensor type")]))))]
    [(_) #'"define-op-dispatch: invalid syntax"]))

(define-op-dispatch mul)
(define-op-dispatch div)
(define-op-dispatch add)
(define-op-dispatch sub)
(define-op-dispatch matmul)

(define (dispatch-equal type iter?)
  (if iter?
      (case (ctype->layout type)
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
         (error "unsupported tensor type")])
      ndarray_equal))

(define-syntax-rule (define-binary-op name dispatch)
  (define (name a b)
    (define type
      (if (tensor? a)
          (tensor-type a)
          (tensor-type b)))
    (define iter? (or (number? a) (number? b) (tensor-iter a) (tensor-iter b)))
    (define result ((dispatch type iter?) (tensor-ndarray-or-iter a type iter?) (tensor-ndarray-or-iter b type iter?)))
    (tensor type (ndarray-dims->shape result) result)))

(define-binary-op t* dispatch-mul)
(define-binary-op t+ dispatch-add)
(define-binary-op t/ dispatch-div)
(define-binary-op t- dispatch-sub)
(define-binary-op t** dispatch-matmul)

(define (t=? a b)
  (define type (tensor-type a))
  (define iter? (or (tensor-iter a) (tensor-iter b)))
  ((dispatch-equal type iter?) (tensor-ndarray-or-iter a type iter?) (tensor-ndarray-or-iter b type iter?)))

(define (tsqrt t)
  (define type (tensor-type t))
  (define shape (tshape t))
  (if (tensor-iter t)
      (case (ctype->layout type)
        [(double) (tensor type shape (ndarray_iter_sqrt_double (tensor-iter t)))]
        [(float) (tensor type shape (ndarray_iter_sqrt_float (tensor-iter t)))]
        [else
         (error "unsupported tensor type" type)])
      (case (ctype->layout type)
        [(double) (tensor type shape (ndarray_sqrt_double (tensor-ndarray t)))]
        [(float) (tensor type shape (ndarray_sqrt_float (tensor-ndarray t)))]
        [else
         (error "unsupported tensor type" type)])))

(define (texpt t w)
  (define type (tensor-type t))
  (define shape (tshape t))
  (if (tensor-iter t)
      (case (ctype->layout type)
        [(double) (tensor type shape (ndarray_iter_expt_double (tensor-iter t) (exact->inexact w)))]
        [(float) (tensor type shape (ndarray_iter_expt_float (tensor-iter t) (exact->inexact w)))]
        [else
         (error "unsupported tensor type" type)])
      (case (ctype->layout type)
        [(double) (tensor type shape (ndarray_expt_double (tensor-ndarray t) (exact->inexact w)))]
        [(float) (tensor type shape (ndarray_expt_float (tensor-ndarray t) (exact->inexact w)))]
        [else
         (error "unsupported tensor type" type)])))

(define (tsum t #:axis [axis #f])
  (cond
    [(false? axis)
     (if (tensor-iter t)
         (case (ctype->layout (tensor-type t))
           [(double) (ndarray_iter_sum_double (tensor-iter t))]
           [(float)  (ndarray_iter_sum_float (tensor-iter t))]
           ['(double double) (ndarray_iter_sum_complex (tensor-iter t))]
           [(int32)  (ndarray_iter_sum_int32_t (tensor-iter t))]
           [(int64)  (ndarray_iter_sum_int64_t (tensor-iter t))]
           [(uint32) (ndarray_iter_sum_uint32_t (tensor-iter t))]
           [(uint64) (ndarray_iter_sum_uint64_t (tensor-iter t))]
           [else
            (error "unsupported tensor type" (tensor-type t))])
         (case (ctype->layout (tensor-type t))
           [(double) (ndarray_sum_double (tensor-ndarray t))]
           [(float)  (ndarray_sum_float (tensor-ndarray t))]
           ['(double double) (ndarray_sum_complex (tensor-ndarray t))]
           [(int32)  (ndarray_sum_int32_t (tensor-ndarray t))]
           [(int64)  (ndarray_sum_int64_t (tensor-ndarray t))]
           [(uint32) (ndarray_sum_uint32_t (tensor-ndarray t))]
           [(uint64) (ndarray_sum_uint64_t (tensor-ndarray t))]
           [else
            (error "unsupported tensor type" (tensor-type t))]))]
    [(>= axis (sub1 (NDArray-ndim (tensor-ndarray t))))
     (error "axis exceeds tensor dimensions minus one")]
    [(false? (tensor-iter t))
     (define axis-sub-elems (tlen t axis))
     (define dataptr (NDArray-dataptr (tensor-ndarray t)))
     (define type (tensor-type t))
     (define result-shape (vector-take (tshape t) (add1 axis)))
     (define result (make-tensor result-shape #:ctype type))
     (define (fill-result shape-to-fill sub-shape result-idx src-idx)
       (cond
         [(= (vector-length shape-to-fill) 1)
          (printf "~a ~a ~a ~a~n" shape-to-fill sub-shape result-idx src-idx)
          (for ([i (in-range result-idx (+ result-idx (vector-ref shape-to-fill 0)))]
                [j (in-naturals 0)])
            (define offset (+ src-idx (* j axis-sub-elems)))
            (printf "  ~a~n" offset)
            (tset! result i
                   (for/sum ([k (in-range 0 axis-sub-elems)])
                     (ptr-ref dataptr type (+ offset k)))))
          result]
         [else
          (for ([i (vector-ref shape-to-fill 0)])
            (fill-result (vector-drop shape-to-fill 1)
                         (vector-drop sub-shape 1)
                         (* i (vector-ref shape-to-fill (sub1 (vector-length shape-to-fill))))
                         (* i (apply * (vector->list sub-shape)))))
          result]))
     (fill-result result-shape (vector-drop (tshape t) 1) 0 0)]
    [(tensor-iter t)
     (error "tsum on iterators not currently supported")]
    [else
     (error "unsupported argument")]))


