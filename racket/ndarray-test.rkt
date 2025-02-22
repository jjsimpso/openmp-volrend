#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/flonum)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt")


#|
(define _shape-array-1d (_array _intptr 1))
(define shape (malloc _shape-array-1d 'atomic))
(array-set! (ptr-ref shape _shape-array-1d 0) 0 10) ; `ptr-ref` instead of `cast`
(array-ref (ptr-ref shape _shape-array-1d 0) 0)
|#

(define (linspace start stop [num 50] [endpoint? #t])
  (define num-points (if endpoint? (add1 num) num))
  (define shape (malloc _intptr 1))
  (ptr-set! shape _intptr 0 num-points)
  (define indexes (ndarray_new 1 (cast shape _pointer _intptr-pointer) (ctype-sizeof _double) #f))
  (ndarray_fill_index_double indexes)
  ;(ptr-ref shape _intptr 0)

  (define scalar_shape (malloc _intptr 1))
  (ptr-set! scalar_shape _intptr 0 1)
  (define start-data (malloc _double 1))
  (ptr-set! start-data _double 0 (->fl start))
  (define stop-data (malloc  _double 1))
  (ptr-set! stop-data _double 0 (->fl (- stop start)))
  (define step-data (malloc _double 1))
  (ptr-set! step-data _double 0 (exact->inexact (/ 1 num)))
  
  (define a-start (ndarray_new 1 (cast scalar_shape _pointer _intptr-pointer) (ctype-sizeof _double) (cast start-data _pointer _uint8-pointer)))
  (define a-span (ndarray_new 1 (cast scalar_shape _pointer _intptr-pointer) (ctype-sizeof _double) (cast stop-data _pointer _uint8-pointer)))
  (define a-step (ndarray_new 1 (cast scalar_shape _pointer _intptr-pointer) (ctype-sizeof _double) (cast step-data _pointer _uint8-pointer)))

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

(define result (linspace 0 5000 50 #t))
(map (lambda (i) (ndarray-ref result _double i)) '(0 1 2 3 4 5 50))
