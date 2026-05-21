#lang racket

(require racket/gui/base
         racket/runtime-path
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/cvector
         plot
         "ndarray-ffi.rkt"
         "tensor.rkt"
         "tensor-geom.rkt")

(define-ndarray ndarray_vol_render_uint8_t (_fun _NDArray-pointer _int _int _int _NDArray-pointer _pointer _pointer _pointer _pointer
                                                 -> (p : _NDArray-pointer/null)
                                                 -> (check-null p 'ndarray_vol_render_uint8_t))
  #:wrap (allocator ndarray_free))

(define (tensor-vol-render vol trans)
  (define shape (tensor-shape vol))
  (ndarray_vol_render_uint8_t (tensor-ndarray vol) (vector-ref shape 2) (vector-ref shape 1) (vector-ref shape 0) (tensor-ndarray trans) #f #f #f #f))

(define (tensor->argb-pixels t)
  (define shape (tensor-shape t))
  (define width (vector-ref shape 1))
  (define height (vector-ref shape 0))
  (define argb-pixels (make-bytes (* width height 4) 0))
  (define dataptr (ptr-add (NDArray-dataptr (tensor-ndarray t)) 0))
  ;; increment through the destination byte string, copying RGB pixel data from the tensor. leave alpha value intact
  (for ([off (in-range 0 (* height width 4) 4)])
    (memcpy argb-pixels (add1 off) dataptr 0 3)
    (ptr-add! dataptr 3))
  argb-pixels)
  
(define (draw-tensor t)
  (define shape (tensor-shape t))
  (define width (vector-ref shape 1))
  (define height (vector-ref shape 0))
  (define target (make-bitmap width height #f))
  (send target set-argb-pixels 0 0 width height (tensor->argb-pixels t))
  target)

(define (tensor-read-ppm path)
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

(define (read-volume path)
  (define (read-int in [signed? #t])
    (integer-bytes->integer (read-bytes 4 in) signed? #t))

  (define (read-float in)
    (floating-point-bytes->real (read-bytes 4 in) #t))
  
  (define in (open-input-file path))
  (unless in
    (error 'read-volume "error reading volume ~a, ~a" path "file not found/accessible"))
  
  (with-handlers ([exn:fail? (lambda (v)
                               (close-input-port in)
                               ((error-display-handler) (exn-message v) v)
                               #f)])
    (define magic (read-int in))
    (unless #t
      (error 'read-volume "error reading volume ~a, ~a" path "not a supported file"))
    (define header-len (read-int in))
    (define width (read-int in))
    (define height (read-int in))
    (define images (read-int in))
    (define bits-per-voxel (read-int in))
    (printf "reading volume: magic=~a, hlen=~a, w=~a, h=~a, images=~a, bits-per-voxel=~a~n" magic header-len width height images bits-per-voxel)
    (define index-bits (read-int in))
    (define scalex (if (> header-len 28) (read-float in) 1.0))
    (define scaley (if (> header-len 32) (read-float in) 1.0))
    (define scalez (if (> header-len 36) (read-float in) 1.0))
    (define rotx (if (> header-len 40) (read-float in) 0.0))
    (define roty (if (> header-len 44) (read-float in) 0.0))
    (define rotz (if (> header-len 48) (read-float in) 0.0))
    (define description
      (if (> header-len 48)
          (bytes->string/utf-8 (read-bytes (- header-len (* 13 4)) in))
          ""))
    (printf "                description=~a~n" description)
    
    (define bytes-per-voxel (ceiling (/ bits-per-voxel 8)))
    (define data (read-bytes (* width height images bytes-per-voxel) in))
    (unless (= (bytes-length data) (* width height images bytes-per-voxel))
      (error 'read-volume "error reading volume ~a, ~a ~a" path "incorrect number of bytes read" (bytes-length data)))

    (printf "opened volume ~a: ~ax~ax~a, bpv=~a~n" path width height images bytes-per-voxel)
    ; todo: set ctype based on bytes-per-voxel
    (define t (make-tensor (vector images height width) data #:ctype _uint8))
    (close-input-port in)
    t))

(define (show-histogram t)
  (define xstart 10)
  (define histogram (make-vector 256 0))
  (for ([val (in-tensor t)])
    (vector-set! histogram val (add1 (vector-ref histogram val))))
  (define pts (for/vector ([i (in-range 0 256)]) (vector i (vector-ref histogram i))))
  (define max
    (for/fold ([m 0])
              ([i (in-naturals)]
               [v (in-vector histogram)]
               #:when (>= i xstart))
      (if (> v m) v m)))
  (plot (lines pts #:x-min xstart #:x-max 255 #:y-min 0 #:y-max max)))

(define (volume-render path trans)
  (define vol (read-volume path))
  (define vol-shape (tshape vol))
  (make-tensor (vector (vector-ref vol-shape 1) (vector-ref vol-shape 2) 3) (tensor-vol-render vol trans)))

(define trans
  (t** (tensor-translate-3d 75.0 75.0 150.0)
       (t** (tensor-rotate-x-3d 150)
            (t** (tensor-rotate-y-3d -20)
                 (tensor-translate-3d -75.0 -75.0 -75.0)))))

(draw-tensor (volume-render "/home/jonathan/coding/volume_rendering/data/engine.vol" (tensor-identity-3d)))
