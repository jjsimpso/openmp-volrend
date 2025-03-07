#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt")

(provide linspace
         run-linspace)

#|
(define _shape-array-1d (_array _intptr 1))
(define shape (malloc _shape-array-1d 'atomic))
(array-set! (ptr-ref shape _shape-array-1d 0) 0 10) ; `ptr-ref` instead of `cast`
(array-ref (ptr-ref shape _shape-array-1d 0) 0)
|#

(define (print-iter it type)
  (let loop ([cursor (NDArrayIter-cursor it)]
             [i 0])
    (printf "it[~a] = ~a~n" i (ptr-ref cursor type 0))
    (when (ndarray_iter_next it)
      (loop (NDArrayIter-cursor it) (add1 i))))
  (ndarray_iter_reset it))

(define (test-simple-iterator)
  (define nda (ndarray_new 2 (vector 10 5) (ctype-sizeof _int) #f))
  (define it (ndarray_iter_new nda #f))
  (let loop ([cursor (NDArrayIter-cursor it)]
             [i 0])
    (ptr-set! cursor _int 0 i)
    (when (ndarray_iter_next it)
      (loop (NDArrayIter-cursor it) (add1 i))))
  (ndarray_iter_reset it)
  (print-iter it _int)

  #;(define it2 (ndarray_iter_new nda (make-Slice 1 8 3) (make-Slice 0 2 2)))
  #;(print-iter it2 _int))

(define (linspace start stop [num 50] [endpoint? #t])
  (define num-points (if endpoint? (add1 num) num))
  (define shape (vector num-points))
  (define indexes (ndarray_new 1 shape (ctype-sizeof _double) #f))
  (ndarray_fill_index_double indexes)

  (define scalar_shape (vector 1))
  (define start-data (cvector _double (->fl start)))
  (define stop-data (cvector _double (->fl (- stop start))))
  (define step-data  (cvector _double (exact->inexact (/ 1 num))))
  
  (define a-start (ndarray_new 1 scalar_shape (ctype-sizeof _double) (cvector-ptr start-data)))
  (define a-span (ndarray_new 1 scalar_shape (ctype-sizeof _double) (cvector-ptr stop-data)))
  (define a-step (ndarray_new 1 scalar_shape (ctype-sizeof _double) (cvector-ptr step-data)))

  ;(define result (ndarray_mul_double indexes a-step))
  #;(list
   (map (lambda (i) (ndarray-ref indexes _double i)) '(0 1 2 3 4))
   (ndarray-ref a-start _double 0)
   (ndarray-ref a-span _double 0)
   (ndarray-ref a-step _double 0)
   (map (lambda (i) (ndarray-ref result _double i)) '(0 1 2 3 4))
   ;(map (lambda (i) (ptr-ref (NDArray-dataptr (ptr-ref result _NDArray)) _double i)) '(0 1 2 3 4))
   (ndarray-dims result 0)
   (NDArray-elem_bytes (ptr-ref result _NDArray)))
   
  (ndarray_add_double a-start (ndarray_mul_double a-span (ndarray_mul_double a-step indexes))))

(define (run-linspace)
  (define result (linspace 0 5000 50 #t))
  (map (lambda (i) (ndarray-ref result _double i)) '(0 1 2 3 4 5 50)))

;(run-linspace)
;(collect-garbage 'major)
