#lang racket

(require ffi/unsafe
         "ndarray-ffi.rkt"
         "ndarray-convolve-ffi.rkt"
         "tensor.rkt")

(provide tensor-convolve2d)

(define (tensor-convolve2d t w h kernel)
  (when (tensor-iter t)
    (error "convolution is not supported on tensor iterators"))
  
  (define shape (tshape t))
  (define type (tensor-type t))
  (case (trank t)
    [(2)
     (case (ctype->layout type)
       [(uint8) (make-tensor (tshape t) (ndarray_convolve2d_uint8_t (tensor-ndarray t) kernel w h) #:ctype _uint8)]
       [(uint16) (make-tensor (tshape t) (ndarray_convolve2d_uint16_t (tensor-ndarray t) kernel w h) #:ctype _uint16)]
       [else
        (error "unsupported tensor type" type)])]
    [(3)
     (case (vector-ref shape 2)
       [(3)
        (case (ctype->layout type)
          [(uint8) (make-tensor (tshape t) (ndarray_convolve2d_vec3_uint8_t (tensor-ndarray t) kernel w h) #:ctype _uint8)]
          [(uint16) (make-tensor (tshape t) (ndarray_convolve2d_vec3_uint16_t (tensor-ndarray t) kernel w h) #:ctype _uint16)]
          [else
           (error "unsupported tensor type" type)])]
       [else
        (error "2D convolution only supports convolution over 3 or 4 planes")])]
    [else
     (error "2D convolution only supported for tensors of 2 or 3 dimensions")]))

