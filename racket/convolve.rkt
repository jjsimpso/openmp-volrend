#lang racket

(require racket/gui/base
         racket/runtime-path
         racket/flonum
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/cvector
         plot
         "ndarray-ffi.rkt"
         "ndarray-convolve-ffi.rkt"
         "tensor.rkt"
         "tensor-image.rkt"
         "tensor-convolve.rkt")

(provide draw-tensor)


(define (draw-tensor t)
  (define shape (tensor-shape t))
  (define width (vector-ref shape 1))
  (define height (vector-ref shape 0))
  (define target (make-bitmap width height #f))
  (send target set-argb-pixels 0 0 width height (tensor->argb-pixels t))
  target)

(define (image-smooth t)
  (define kernel (cvector _double
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)))
  (tensor-convolve2d t 3 3 kernel #:mode 'NDARRAY_CONVOLVE_CLAMP))

(define (image-super-smooth t)
  (define kernel (cvector _double
                          (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25)
                          (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25)
                          (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25)
                          (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25)
                          (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25) (exact->inexact 1/25)))
  (tensor-convolve2d t 5 5 kernel #:mode 'NDARRAY_CONVOLVE_CLAMP))

(define (image-smooth-slow t)
  (define kernel (cvector _double
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)))
  (define t2 (tensor-copy t))
  (define shape (tensor-shape t2))
  (define width (vector-ref shape 1))
  (define height (vector-ref shape 0))
  #;(define cursor (ptr-add (NDArray-dataptr (tensor-ndarray t2)) 0))
  (for* ([y (in-range 2 (- height 2))]
         [x (in-range 2 (- width 2))])
    #;(when (and (< y 5) (< x 5))
      (printf "~ax~a ~a -> ~a " x y (ndarray-ref (tensor-ndarray t) _uint8 y x) (ndarray_convolve2d_point_uint8_t (tensor-ndarray t2) kernel 3 3 x y)))
    #;(ptr-set! cursor _uint8 (ndarray_convolve2d_point_uint8_t (tensor-ndarray t) kernel 3 3 x y))
    (ndarray-set! (tensor-ndarray t2) _uint8 y x (ndarray_convolve2d_point_uint8_t (tensor-ndarray t) kernel 3 3 x y))
    #;(when (and (< y 5) (< x 5))
      (printf " actual=~a~n" (ndarray-ref (tensor-ndarray t2) _uint8 y x)))
    #;(ptr-add! cursor 1))
  t2)

(define (image3-smooth-slow t)
  (define kernel (cvector _double
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)
                          (exact->inexact 1/9) (exact->inexact 1/9) (exact->inexact 1/9)))
  (define t2 (tensor-copy t))
  (define shape (tensor-shape t2))
  (define width (vector-ref shape 1))
  (define height (vector-ref shape 0))
  (define rgb (malloc _uint8 3))
  #;(define cursor (ptr-add (NDArray-dataptr (tensor-ndarray t2)) 0))
  (for* ([y (in-range 2 (- height 2))]
         [x (in-range 2 (- width 2))])
    (ndarray_convolve2d_point_vec3_uint8_t (tensor-ndarray t) kernel 3 3 x y rgb)
    #;(when (and (< y 5) (< x 5))
        (printf "~ax~a ~a -> ~a ~a ~a " x y (ndarray-ref (tensor-ndarray t) _uint8 y x) (ptr-ref rgb _uint8 0) (ptr-ref rgb _uint8 1) (ptr-ref rgb _uint8 2)))
    ;(tset! t2 y x 0 (ptr-ref rgb _uint8 0))
    ;(tset! t2 y x 1 (ptr-ref rgb _uint8 1))
    ;(tset! t2 y x 2 (ptr-ref rgb _uint8 2))
    (ndarray-set! (tensor-ndarray t2) _uint8 y x 0 (ptr-ref rgb _uint8 0))
    (ndarray-set! (tensor-ndarray t2) _uint8 y x 1 (ptr-ref rgb _uint8 1))
    (ndarray-set! (tensor-ndarray t2) _uint8 y x 2 (ptr-ref rgb _uint8 2)))
  t2)

;(image-smooth (tensor-read-pgm "../data/dosboxes.pgm"))

