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

(define (test-tmap)
  (define t1 (make-tensor (vector 3 3) 'index #:ctype _uint8))
  (define t2 (tmap (lambda (x) (* x 2)) t1))
  (check-equal?
   (map (lambda (i) (tref t2 i)) '(0 1 2 3 4 5 6 7 8))
   '(0 2 4 6 8 10 12 14 16))
  (define t3 (tmap (lambda (x y) (+ x y)) t1 t2))
  (check-equal?
   (map (lambda (i) (tref t3 i)) '(0 1 2 3 4 5 6 7 8))
   '(0 3 6 9 12 15 18 21 24)))

(define (test-build-tensor)
  (define t1 (build-tensor (vector 3 3) (lambda (i) (exact->inexact i)) _float))
  (check-equal?
   (map (lambda (i) (tref t1 i)) '(0 1 2 3 4 5 6 7 8))
   '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0)))

(define (run-tests)
  (test-linspace)
  (test-ppm)
  (test-tmap)
  (test-build-tensor))

(run-tests)
