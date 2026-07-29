#lang racket

(require ffi2
         "ndarray-ffi.rkt"
         "tensor.rkt"
         "convolve.rkt")

(require (only-in ffi/unsafe _uint8))

(define libvolrend (ffi2-lib "../libvolrend"))
(define-ffi2-definer define-ndarray #:lib libvolrend)

(define-ndarray ndarray_convolve2d_uint8_t (ptr_t (array_t double_t *) int_t int_t intptr_t intptr_t . -> . uint8_t))

(define (image-smooth t)
  (define t2 (tensor-copy t))
  (define shape (tensor-shape t2))
  (define width (vector-ref shape 1))
  (define height (vector-ref shape 0))
  (define kernel (ffi2-malloc double_t 9))
  (for ([i (in-range 0 9)])
    (ffi2-set! kernel double_t i (exact->inexact 1/9)))
  (for* ([y (in-range 2 (- height 2))]
         [x (in-range 2 (- width 2))])
    (ndarray-set! (tensor-ndarray t2) _uint8 y x (ndarray_convolve2d_uint8_t (cpointer->ptr_t (tensor-ndarray t)) kernel 3 3 x y)))
  t2)
