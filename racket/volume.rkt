#lang racket

(require racket/gui/base
         ffi/unsafe
         ffi/unsafe/alloc
         plot
         "ndarray-ffi.rkt"
         "tensor.rkt"
         "tensor-geom.rkt")

(define-cstruct _Vec3_double
  ([x _double]
   [y _double]
   [z _double]))

(define-cstruct _Vec4_double
  ([x _double]
   [y _double]
   [z _double]
   [w _double]))

(define-cstruct _Rgba
  ([r _double]
   [g _double]
   [b _double]
   [a _double]))

(define-cstruct _Material
  ([min _int]
   [max _int]
   [amb _double]
   [diff _double]
   [spec _double]
   [r _double]
   [g _double]
   [b _double]
   [a _double]))

(define-cstruct _ClassifyInfo
  ([num_mat _int]
   [mat _Material-pointer]))

(define _grad_fun (_fun _NDArray-pointer _intptr _intptr _intptr -> _Vec3_double))
(define _class_fun (_fun _int _Vec3_double _ClassifyInfo-pointer -> _Material-pointer/null))
(define _interp_fun (_fun _NDArray-pointer _Vec4_double-pointer -> _uint8))

;; get the addresses of these functions to use as function pointers
(define libvolrend (ffi-lib "../libvolrend"))
(define ndarray_vol_central_diff_uint8_t (get-ffi-obj "ndarray_vol_central_diff_uint8_t" libvolrend _fpointer))
(define ndarray_vol_classify_simple_uint8_t (get-ffi-obj "ndarray_vol_classify_simple_uint8_t" libvolrend _fpointer))
(define ndarray_vol_interp_nearest_uint8_t (get-ffi-obj "ndarray_vol_interp_nearest_uint8_t" libvolrend _fpointer))
(define ndarray_vol_interp_linear_uint8_t (get-ffi-obj "ndarray_vol_interp_linear_uint8_t" libvolrend _fpointer))

#| 
;; don't want to call these directly. use function pointers above.
(define-ndarray ndarray_vol_central_diff_uint8_t (_fun _NDArray-pointer _intptr _intptr _intptr -> _Vec3_double))
(define-ndarray ndarray_vol_classify_simple_uint8_t (_fun _uint8 _Vec3_double _ClassifyInfo-pointer -> _Material-pointer/null))
(define-ndarray ndarray_vol_interp_nearest_uint8_t (_fun _NDArray-pointer _Vec4_double-pointer -> _uint8))
(define-ndarray ndarray_vol_interp_linear_uint8_t (_fun _NDArray-pointer _Vec4_double-pointer -> _uint8))
|#

(define-ndarray ndarray_vol_mip_uint8_t (_fun _NDArray-pointer _int _int _int _NDArray-pointer
                                                 -> (p : _NDArray-pointer/null)
                                                 -> (check-null p 'ndarray_vol_mip_uint8_t))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_vol_render_uint8_t (_fun _NDArray-pointer _int _int _int _NDArray-pointer _double _pointer _pointer _ClassifyInfo-pointer _pointer
                                                 -> (p : _NDArray-pointer/null)
                                                 -> (check-null p 'ndarray_vol_render_uint8_t))
  #:wrap (allocator ndarray_free))


(define (tensor-vol-mip vol trans)
  (define shape (tensor-shape vol))
  (ndarray_vol_mip_uint8_t (tensor-ndarray vol) (vector-ref shape 2) (vector-ref shape 1) (vector-ref shape 0) (tensor-ndarray trans)))

(define (tensor-vol-render vol trans persp-dist mat-list)
  (define shape (tensor-shape vol))
  (define mats (malloc _Material (length mat-list))) ;; 0 - 99 0.0, 100 - 170 0.1, 171 - 184 0.0, 185 - 235 0.9, 236 - 255 0.0
  (for ([e (in-list mat-list)]
        [i (in-naturals)])
    (ptr-set! mats _Material i e))
  (set-cpointer-tag! mats 'Material)
  (define cinfo (make-ClassifyInfo (length mat-list) mats))
  (ndarray_vol_render_uint8_t (tensor-ndarray vol) (vector-ref shape 2) (vector-ref shape 1) (vector-ref shape 0) (tensor-ndarray trans) persp-dist
                              ndarray_vol_central_diff_uint8_t
                              ndarray_vol_classify_simple_uint8_t
                              cinfo
                              ndarray_vol_interp_linear_uint8_t))

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
  (black-box t)
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

(define (volume-mip path trans)
  (define vol (read-volume path))
  (define vol-shape (tshape vol))
  (make-tensor (vector (vector-ref vol-shape 1) (vector-ref vol-shape 2) 3) (tensor-vol-mip vol trans)))

(define (volume-render path trans mat-list #:persp-dist [persp-dist 0.0])
  (define vol (read-volume path))
  (define vol-shape (tshape vol))
  ;; make tensor representing an RGB image of the rendered volume
  (make-tensor (vector (vector-ref vol-shape 1) (vector-ref vol-shape 2) 3) (tensor-vol-render vol trans persp-dist mat-list)))

(define trans
  (t** (tensor-translate-3d 128.0 128.0 55.0)
       (t** (tensor-rotate-x-3d 150)
            (t** (tensor-rotate-y-3d -20)
                 (tensor-translate-3d -128.0 -128.0 -55.0)))))

(define roty (t** (tensor-translate-3d 128.0 128.0 55.0)
                  (t** (tensor-rotate-y-3d 90)
                       (tensor-translate-3d -128.0 -128.0 -55.0))))

(define roty45 (t** (tensor-translate-3d 128.0 128.0 55.0)
                    (t** (tensor-rotate-y-3d 45)
                         (tensor-translate-3d -128.0 -128.0 -55.0))))

(define engine-mats (list 
                     (make-Material 0    99 0.1 0.9 0.0 1.0 1.0 1.0 0.0)
                     (make-Material 100 170 0.1 0.9 0.0 0.7 0.7 0.7 0.05)
                     (make-Material 171 184 0.1 0.9 0.0 1.0 1.0 1.0 0.0)
                     (make-Material 185 235 0.1 0.9 0.0 0.5 0.0 0.0 0.9)
                     (make-Material 236 255 0.1 0.9 0.0 1.0 1.0 1.0 0.0)))

(define cthead-mats (list
                     (make-Material 0    75 0.1 0.9 0.0 0.0 0.0 0.0 0.0)
                     (make-Material 76   95 0.1 0.9 0.0 0.5 0.5 0.5 0.5)
                     (make-Material 96  255 0.1 0.9 0.0 0.0 0.0 0.0 0.0)))

(define mrbrain-mats (list
                      (make-Material 0    45 0.1 0.9 0.0 0.0 0.0 0.0 0.0)
                      (make-Material 46   90 0.1 0.9 0.0 0.5 0.5 0.5 0.5)
                      (make-Material 91  105 0.1 0.9 0.0 0.0 0.7 0.0 0.5)
                      (make-Material 106 255 0.1 0.9 0.0 0.0 0.0 0.0 0.0)))

;(draw-tensor (volume-mip "/home/jonathan/coding/volume_rendering/data/engine.vol" roty))
;(draw-tensor (volume-render "/home/jonathan/coding/volume_rendering/data/engine.vol" roty45 engine-mats #:persp-dist 4096.0))
;(draw-tensor (volume-render "/home/jonathan/coding/volume_rendering/data/engine.vol" (tensor-identity-3d)))
;(draw-tensor (volume-render "/home/jonathan/coding/volume_rendering/physics/gvs/datasets/3dhead.vol" (tensor-identity-3d)))
