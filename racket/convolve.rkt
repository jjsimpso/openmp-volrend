#lang racket

(require racket/gui/base
         racket/runtime-path
         racket/flonum
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/cvector
         plot
         "ndarray-ffi.rkt"
         "tensor.rkt"
         "tensor-geom.rkt")

(define libvolrend (ffi-lib "../libvolrend"))

(define-ndarray ndarray_convolve2d_uint8_t (_fun _NDArray-pointer _cvector _int _int _intptr _intptr -> _uint8))

(define (tensor-read-pgm path)
  (define (whitespace? b)
    (if (eof-object? b)
        #f
        (or (= b 32)
            (and (>= b 9) (<= b 13)))))
  
  (define (discard-whitespace in)
    (when (whitespace? (peek-byte in))
      (read-byte in)
      (discard-whitespace in)))

  (define (skip-whitespace in)
    (define ws? (whitespace? (peek-byte in)))
    (discard-whitespace in)
    ws?)

  (define in (open-input-file path))
  (unless in
    (error 'read-pgm "error reading pgm, ~a" "file not found/accessible"))
  
  (with-handlers ([exn:fail? (lambda (v)
                               (close-input-port in)
                               ((error-display-handler) (exn-message v) v)
                               #f)])
    (define magic (read-bytes 2 in))
    (unless (and (bytes=? magic #"P5") (skip-whitespace in))
      (error 'read-pgm "error reading pgm, ~a" "not a supported file"))
    (define w (read in))
    (unless (and (exact-integer? w) (skip-whitespace in))
      (error 'read-pgm "error reading pgm, ~a" "no width read"))
    (define h (read in))
    (unless (and (exact-integer? h) (skip-whitespace in))
      (error 'read-pgm "error reading pgm, ~a" "no height read"))
    (define maxval (read in))
    (unless (and (exact-integer? maxval) (skip-whitespace in))
      (error 'read-pgm "error reading pgm, ~a" "no maxval read"))
    ;; assuming maxval of 255 or less, so 1 byte per pixel
    (define data (read-bytes (* w h) in))
    (unless (= (bytes-length data) (* w h))
      (error 'read-pgm "error reading pgm, ~a" "incorrect number of bytes read"))

    ;(printf "opened pgm ~ax~a, max ~a~n" w h maxval)
    (define t (make-tensor (vector h w) data #:ctype _uint8))
    (close-input-port in)
    t))

(define (tensor->argb-pixels t)
  (define shape (tensor-shape t))
  (define dims (vector-length shape))
  (define width (vector-ref shape 1))
  (define height (vector-ref shape 0))
  (define argb-pixels (make-bytes (* width height 4) 0))
  (define dataptr (ptr-add (NDArray-dataptr (tensor-ndarray t)) 0))
  (cond
    [(= dims 2)
     ;; increment through the destination byte string, copying grayscale pixel data from the tensor. leave alpha value intact
     (for ([off (in-range 0 (* height width 4) 4)])
       (memcpy argb-pixels (+ off 1) dataptr 0 1)
       (memcpy argb-pixels (+ off 2) dataptr 0 1)
       (memcpy argb-pixels (+ off 3) dataptr 0 1)
       (ptr-add! dataptr 1))]
    [(and (= dims 3) (= (vector-ref shape 2) 3))
     ;; increment through the destination byte string, copying RGB pixel data from the tensor. leave alpha value intact
     (for ([off (in-range 0 (* height width 4) 4)])
       (memcpy argb-pixels (add1 off) dataptr 0 3)
       (ptr-add! dataptr 3))]
    [else
     (error 'tensor->argb-pixels "unsupported depth in source tensor")])
  (black-box t)
  argb-pixels)

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
  (define t2 (tensor-copy t))
  (define shape (tensor-shape t2))
  (define width (vector-ref shape 1))
  (define height (vector-ref shape 0))
  (define cursor (ptr-add (NDArray-dataptr (tensor-ndarray t2)) 0))
  (for* ([y (in-range 2 (- height 2))]
         [x (in-range 2 (- width 2))])
    #;(when (and (< y 5) (< x 5))
      (printf "~ax~a ~a -> ~a " x y (ndarray-ref (tensor-ndarray t) _uint8 y x) (ndarray_convolve2d_uint8_t (tensor-ndarray t2) kernel 3 3 x y)))
    ;(ptr-set! cursor _uint8 (ndarray_convolve2d_uint8_t (tensor-ndarray t) kernel 3 3 x y))
    (ndarray-set! (tensor-ndarray t2) _uint8 y x (ndarray_convolve2d_uint8_t (tensor-ndarray t) kernel 3 3 x y))
    #;(when (and (< y 5) (< x 5))
      (printf " actual=~a~n" (ndarray-ref (tensor-ndarray t2) _uint8 y x)))
    (ptr-add! cursor 1))
  t2)

;(image-smooth (tensor-read-pgm "../data/dosboxes.pgm"))
