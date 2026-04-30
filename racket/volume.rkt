#lang racket

(require racket/runtime-path
         ffi/unsafe
         plot
         "tensor.rkt")


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
