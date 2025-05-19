#lang racket

(require racket/flonum
         racket/runtime-path
         ffi/unsafe
         "ndarray-ffi.rkt"
         "ndarray-io-ffi.rkt"
         "tensor.rkt")

(require rackunit)

(define (print-tensor t)
  (for ([x (in-tensor t)]
        [i (in-naturals 0)])
    (printf "t[~a] = ~a~n" i x)))

(define (linspace start stop [num 50] [endpoint? #t])
  (define num-points (if endpoint? (add1 num) num))
  (define shape (vector num-points))
  (define indexes (make-tensor shape 'index #:ctype _double))

  (define a-start (make-tensor (vector 1) (->fl start) #:ctype _double))
  (define a-span (make-tensor (vector 1)  (->fl (- stop start)) #:ctype _double))
  (define a-step (make-tensor (vector 1)  (exact->inexact (/ 1 num)) #:ctype _double))

  (t+ a-start (t* a-span (t* a-step indexes))))

#;(define (plot-observations)
  (define xs (linspace 500 5000 20 #t))
  (define ys (t* (make-tensor (vector 1) 1.5) (texpt xs 3.5)))
  
  void)

(define (test-linspace)
  (define result (linspace 0 5000 50 #t))
  (check-equal?
   (map (lambda (i) (tref result i)) '(0 1 2 3 4 5 50))
   '(0.0 100.0 200.0 300.0 400.0 500.0 5000.0)))

(define-runtime-path test-ppm-path "../test/image-01.ppm")
(define-runtime-paths (out-ppm-path out2-ppm-path out3-ppm-path out4-ppm-path)
  (values "tout.ppm" "tout2.ppm" "tout3.ppm" "tout4.ppm"))

(define (tensor-write-ppm t path)
  (define w (vector-ref (tshape t) 1))
  (define h (vector-ref (tshape t) 0))
  (if (tensor-iter t)
      (ndarray_iter_write_ppm (tensor-iter t) path w h)
      (ndarray_write_ppm (tensor-ndarray t) path))
  void)

(define (tensor-read-ppm path)
  (define nda (ndarray_read_ppm path))
  (make-tensor (vector (ndarray-dims nda 1) (ndarray-dims nda 0) 3) nda))
  
(define (test-ppm)
  (define ppm (tensor-read-ppm test-ppm-path))
  (define w (vector-ref (tshape ppm) 1))
  (define h (vector-ref (tshape ppm) 0))
  (tensor-write-ppm ppm out-ppm-path)
  ;(print-tensor ppm)
  ;; writing a ppm with skip dim set like this is no longer supported by ndarray_iter_write_file
  #;(tensor-write-ppm (tslice ppm '() #:skip-dim -1) out2-ppm-path)
  (tensor-write-ppm (tslice ppm `((0 ,(sub1 h)  2)
                                  (0 ,(sub1 w) 2)
                                  (0 2 1)))
                    out3-ppm-path)
  #;(tensor-write-ppm (tslice ppm
                            `((0 ,(sub1 h)  2)
                              (0 ,(sub1 w) 2))
                            #:skip-dim -1)
                    out4-ppm-path))


(define (run-tests)
  (test-linspace)
  (test-ppm))

(run-tests)
