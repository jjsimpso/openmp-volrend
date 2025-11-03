#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum)

(require (for-syntax syntax/parse))

(provide define-ndarray
         _intptr-pointer
         _uint8-pointer
         check-null
         ndarray_new
         ndarray_free
         ndarray_copy
         ndarray_set_freedata
         ndarray_shape_equal
         ndarray_data_equal
         ndarray_iter_new
         ndarray_iter_new_all_but_axis
         ndarray_iter_free
         ndarray_iter_next
         ndarray_iter_reset
         ndarray-ref
         ndarray-set!
         ndarray-dims
         ndarray-dims->shape
         ndarray-sub-elems
         ndarray-data->list
         ndarray-iter-data
         ndarray-iter-data-advance
         ndarray-iter-dims
         ndarray-iter-sub-elems
         make-slice
         in-iter
         _NDArray-pointer
         _NDArray-pointer/null
         _NDArrayIter-pointer
         _NDArrayIter-pointer/null
         _Slice-pointer
         (struct-out NDArray)
         (struct-out NDArrayIter)
         (struct-out Slice))

(define-ffi-definer define-ndarray (ffi-lib "../libvolrend"))

(define MAX-DIMS 32)

(provide _complex)
(provide (struct-out Complex))

(define-cstruct _Complex
  ([real _double]
   [imag _double]))

(define _complex (make-ctype _Complex
                             (lambda (c)
                               (cond
                                 [(and (flonum? (real-part c)) (flonum? (imag-part c)))
                                  (make-Complex (flreal-part c) (flimag-part c))]
                                 [else
                                  (make-Complex (exact->inexact (real-part c)) (exact->inexact (imag-part c)))]))
                             (lambda (v) (make-flrectangular (Complex-real v) (Complex-imag v)))
                             #;(lambda (v) v)))

(define _intptr-pointer (_cpointer 'intptr_t))
(define _uint8-pointer (_cpointer/null 'uint8_t))

(define _intptr-array-max-dims (_array _intptr MAX-DIMS))

(define _NDArray-pointer-pointer (_cpointer '_NDArray-pointer))
(define _NDArrayIter-pointer-pointer (_cpointer '_NDArrayIter-pointer))

(define-cstruct _NDArray
  ([ndim _int]
   [dims _intptr-pointer]
   [num_elems _intptr]
   [elem_bytes _intptr]
   [size _intptr]
   [free_data _stdbool]
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
   [contiguous _stdbool]))

(define-cstruct _NDArrayMultiIter
  ([num _int]
   [nd_m1 _int]
   [index _intptr]
   [length _intptr]
   [dims_m1 _intptr-array-max-dims]
   [nda _NDArray-pointer-pointer]
   [iter _NDArrayIter-pointer-pointer]))

(define-cstruct _Slice
  ([start _intptr]
   [end _intptr]
   [stride _intptr]))

(define (check-null p who)
  (if (false? p)
      (error who "returned NULL")
      p))

;; ND Array functions
;; ------------------
(define-ndarray ndarray_free (_fun _NDArray-pointer -> _void)
  #:wrap (deallocator))
(define-ndarray ndarray_new (_fun _int [dims : (_vector i _intptr)] _intptr _pointer
                                  -> (p : _NDArray-pointer/null)
                                  -> (check-null p 'ndarray_new))
  #:wrap (allocator #:merely-uninterruptible? #t ndarray_free))

(define-ndarray ndarray_copy (_fun _NDArray-pointer
                                   -> (p : _NDArray-pointer/null)
                                   -> (check-null p 'ndarray_copy))
  #:wrap (allocator #:merely-uninterruptible? #t ndarray_free))

(define-ndarray ndarray_set_freedata (_fun _NDArray-pointer _stdbool -> _void))

(define-ndarray ndarray_shape_equal (_fun _NDArray-pointer _NDArray-pointer -> _stdbool))
(define-ndarray ndarray_data_equal (_fun _NDArray-pointer _NDArray-pointer -> _stdbool))

;; ND Array Iterator functions
;; ---------------------------
(define-ndarray ndarray_iter_free (_fun _NDArrayIter-pointer -> _void)
  #:wrap (deallocator))
(define-ndarray ndarray_iter_new (_fun _NDArray-pointer _Slice-pointer/null
                                       -> (p : _NDArrayIter-pointer/null)
                                       -> (check-null p 'ndarray_iter_new))
  #:wrap (allocator #:merely-uninterruptible? #t ndarray_iter_free))
(define-ndarray ndarray_iter_new_all_but_axis (_fun _NDArray-pointer _Slice-pointer/null _pointer
                                                    -> (p : _NDArrayIter-pointer/null)
                                                    -> (check-null p 'ndarray_iter_new_all_but_axis))
  #:wrap (allocator #:merely-uninterruptible? #t ndarray_iter_free))

(define-ndarray ndarray_iter_next (_fun _NDArrayIter-pointer -> _stdbool))
(define-ndarray ndarray_iter_reset (_fun _NDArrayIter-pointer -> _void))


;; ND Array Multi Iterator functions
;; ---------------------------------
(define-ndarray ndarray_multi_iter_free (_fun _NDArrayMultiIter-pointer -> _void)
  #:wrap (deallocator))
(define-ndarray ndarray_multi_iter_new (_fun #:varargs-after 1 _int _NDArray-pointer _NDArray-pointer
                                             -> (p : _NDArrayMultiIter-pointer/null)
                                             -> (check-null p 'ndarray_multi_iter_new))
  #:wrap (allocator ndarray_multi_iter_free))
(define-ndarray ndarray_multi_iter_free_except_iter (_fun _NDArrayMultiIter-pointer -> _void)
  #:wrap (deallocator))
(define-ndarray ndarray_multi_iter_new_from_iter (_fun #:varargs-after 1 _int _NDArrayIter-pointer _NDArrayIter-pointer
                                             -> (p : _NDArrayMultiIter-pointer/null)
                                             -> (check-null p 'ndarray_multi_iter_new_from_iter))
  #:wrap (allocator ndarray_multi_iter_free_except_iter))


;; accessor function for NDArray dims array
;; i is a 0-based index
(define (ndarray-dims p i)
  (when (> (add1 i) (NDArray-ndim p))
    (error (format "dimension ~a exceeds NDArray's dimensions" i)))
  (ptr-ref (NDArray-dims p) _intptr i))

;; calculate the stride of dimension n
;; n is a 0-based index
(define (dim-stride p n)
  (when (> (add1 n) (NDArray-ndim p))
    (error (format "dimension ~a exceeds NDArray's dimensions" n)))
  (define elem-bytes (NDArray-elem_bytes p))
  (for/fold ([sum elem-bytes])
            ([i (in-range (sub1 (NDArray-ndim p)) n -1)])
    (* (ndarray-dims p i) sum)))

;; calculate the number of elements of the subarray within/underneath dimension n
(define (ndarray-sub-elems p n)
  ;(/ (dim-stride p n) (NDArray-elem_bytes p))
  (when (> (add1 n) (NDArray-ndim p))
    (error (format "dimension ~a exceeds NDArray's dimensions" n)))
  (for/fold ([num-elements 1])
            ([i (in-range n (sub1 (NDArray-ndim p)))])
    (* num-elements (ndarray-dims p (add1 i)))))

(define-syntax ndarray-ref
  (syntax-rules ()
    [(ndarray-ref p type i)
     (ptr-ref (NDArray-dataptr p) type i)]
    [(ndarray-ref p type i j)
     (ptr-ref (NDArray-dataptr p)
              type
              'abs
              (+ (* i (dim-stride p 0))
                 (* j (dim-stride p 1))))]
    ;; this case duplicates work in stride calculation
    [(ndarray-ref p type i ...)
     (let* ([indices '(i ...)]
            [offset (for/fold ([sum 0])
                              ([idx (in-list indices)]
                               [dim (in-naturals)])
                      (+ sum (* idx (dim-stride p dim))))])
       (ptr-ref (NDArray-dataptr p) type 'abs offset))]))

;; currently only supports up to 2 dimensions
(define-syntax ndarray-set!
  (syntax-rules ()
    [(ndarray-set! p type i v)
     (ptr-set! (NDArray-dataptr p) type i v)]
    [(ndarray-set! p type i j v)
     (ptr-set! (NDArray-dataptr p)
               type
               'abs
               (+ (* i (dim-stride p 0))
                  (* j (dim-stride p 1)))
               v)]
    [(ndarray-set! p type i ... v)
     (let* ([indices '(i ...)]
            [offset (for/fold ([sum 0])
                              ([idx (in-list indices)]
                               [dim (in-naturals)])
                      (+ sum (* idx (dim-stride p dim))))])
       (ptr-set! (NDArray-dataptr p) type 'abs offset v))]))

(define (ndarray-dims->shape nda)
  (for/vector #:length (NDArray-ndim nda)
              ([i (in-naturals 0)])
    (ndarray-dims nda i)))

(define (ndarray-data->list nda type)
  (cblock->list (NDArray-dataptr nda) type (NDArray-num_elems nda)))

(define-syntax-rule (ndarray-iter-data p type)
  (ptr-ref (NDArrayIter-cursor p) type 0))

(define (ndarray-iter-data-advance p type)
  (define val (ndarray-iter-data p type))
  (ndarray_iter_next p)
  val)

;; return size of dimension i in iterator
(define (ndarray-iter-dims p i)
  (add1 (array-ref (NDArrayIter-dims_m1 p) i)))

;; calculate the number of elements of the subarray within/underneath dimension n
(define (ndarray-iter-sub-elems p n)
  (when (> n (NDArrayIter-nd_m1 p))
    (error (format "dimension ~a exceeds NDArray Iterator's dimensions" n)))
  (for/fold ([num-elements 1])
            ([i (in-range n (NDArrayIter-nd_m1 p))])
    (* num-elements (ndarray-iter-dims p (add1 i)))))

;; provide the number of dimensions and a list of triples(start end stride)
;; returns a cpointer to an array of _Slices
(define (make-slice nd range-lst)
  (define slice-ptr 
    (vector->cblock 
     (for/vector #:length nd
                 ([r (in-list range-lst)])
       (apply make-Slice r))
     _Slice))
  (set-cpointer-tag! slice-ptr 'Slice)
  slice-ptr)

;; for now assume the type is _double in the expression case(non-for)
(define (in-iter/proc n)
  (for/list ([v (in-iter n _double)])
    v))

(define-sequence-syntax in-iter
  (lambda () #'in-iter/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(val) (_ expr type)]
       #'[(val)
          (:do-in
           ([(it) expr])
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
