#lang racket

(require ffi/unsafe
         "ndarray-ffi.rkt"
         "tensor.rkt")

(provide tensor-read-pgm
         tensor-read-ppm
         tensor->argb-pixels)

;; helper functions for reading ppm and pgm files
;; -------------------------------------------
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
;; -------------------------------------------


(define (tensor-read-pgm path)
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

(define (tensor-read-ppm path)
  (define in (open-input-file path))
  (unless in
    (error 'read-ppm2 "error reading ppm, ~a" "file not found/accessible"))
  
  (with-handlers ([exn:fail? (lambda (v)
                               (close-input-port in)
                               ((error-display-handler) (exn-message v) v)
                               #f)])
    (define magic (read-bytes 2 in))
    (unless (and (bytes=? magic #"P6") (skip-whitespace in))
      (error 'read-ppm2 "error reading ppm, ~a" "not a supported file"))
    (define w (read in))
    (unless (and (exact-integer? w) (skip-whitespace in))
      (error 'read-ppm2 "error reading ppm, ~a" "no width read"))
    (define h (read in))
    (unless (and (exact-integer? h) (skip-whitespace in))
      (error 'read-ppm2 "error reading ppm, ~a" "no height read"))
    (define maxval (read in))
    (unless (and (exact-integer? maxval) (skip-whitespace in))
      (error 'read-ppm2 "error reading ppm, ~a" "no maxval read"))
    (define data (read-bytes (* w h 3) in))
    (unless (= (bytes-length data) (* w h 3))
      (error 'read-ppm2 "error reading ppm, ~a" "incorrect number of bytes read"))

    ;(printf "opened ppm ~ax~a, max ~a~n" w h maxval)
    (define t (make-tensor (vector h w 3) data #:ctype _uint8))
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
