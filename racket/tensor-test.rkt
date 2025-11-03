#lang racket

(require racket/flonum
         racket/runtime-path
         ffi/unsafe
         racket/future
         future-visualizer
         "ndarray-ffi.rkt"
         "ndarray-io-ffi.rkt"
         "tensor.rkt"
         "tensor-geom.rkt")

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

(require math/distributions
         math/statistics
         plot)

(define (plot-observations)
  (define (the-process x)
    (* 1.5 (expt x 3.5)))
  (define xs (linspace 500 5000 20 #t))
  (define ys (t* (make-tensor (vector 1) 1.5) (texpt xs 3.5)))
  (define errors
    (let ([d (normal-dist 0 0.25)])
      (build-tensor (tshape ys) (lambda (_i) (sample d)) _double)))
  (printf "xs: shape=~a~n" (tshape xs))
  (print-tensor xs)
  (printf "ys: shape=~a~n" (tshape ys))
  (print-tensor ys)
  (printf "errors: shape=~a~n" (tshape errors))
  (print-tensor errors)
  (define ys-observations (t* ys (t+ (make-tensor (vector 1) 1.0) errors)))
  (define observations (tmap/vector (lambda (x y) (vector x y)) xs ys-observations))
  (define renderers1
    (list
     (tick-grid)
     (function the-process #:color "orange" #:label "the process")
     (points (in-vector observations) #:color "indianred" #:label "observations")))
  (list
  (parameterize ([plot-title "Linear Grid"])
    (plot-pict renderers1))
  (parameterize ([plot-title "Log-Log Grid"]
                 [plot-x-transform log-transform]
                 [plot-y-transform log-transform])
    (plot-pict renderers1))))

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
  (make-tensor (vector (ndarray-dims nda 0) (ndarray-dims nda 1) 3) nda))
  
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

(define (test-ops)
  (define a (make-tensor (vector 3 3 3) 'index))
  (define b (make-tensor (vector 3 3 3) 2.0))
  (define c (t* a b))
  (define d (t* a (make-tensor (vector 1) 2.0)))
  
  (check-equal? (t=? c d) #t)
  (check-equal? (t=? (tslice c '() #:skip-dim 0) (tslice d '() #:skip-dim 0))
                #t)
  (check-equal? (t=? (tslice c '() #:skip-dim 1) (tslice d '() #:skip-dim 0))
                #f)
  (check-equal? (t=? (tslice b '() #:skip-dim 2) (make-tensor (vector 3 3) 2.0))
                #t)
  (check-equal? (t=? (make-tensor (vector 3 3) 2.0) (tslice b '() #:skip-dim 2))
                #t)
  
  (check-equal? (in-tensor (tslice a '() #:skip-dim 0))
                #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
  (check-equal? (in-tensor (tslice a '() #:skip-dim 1))
                #(0.0 1.0 2.0 9.0 10.0 11.0 18.0 19.0 20.0))
  (check-equal? (in-tensor (tslice a '() #:skip-dim 2))
                #(0.0 3.0 6.0 9.0 12.0 15.0 18.0 21.0 24.0))
  
  (check-equal? (t=? c (t* a (tslice b '() #:skip-dim 2)))
                #t)
  (check-equal? (t=? c (t* a (tslice b '() #:skip-dim 1)))
                #t)
  (check-equal? (t=? c (t* a (tslice b '() #:skip-dim 0)))
                #t)
  (check-equal? (t=? c (t* a (make-tensor (vector 3 3) 2.0)))
                #t)
  
  (check-equal? (t=? c (t* b a))
                #t)
  (check-equal? (t=? c (t* (tslice b '() #:skip-dim 0) a))
                #t)
  (check-equal? (t=? c (t* (tslice b '() #:skip-dim 1) a))
                #t)
  (check-equal? (t=? c (t* (tslice b '() #:skip-dim 2) a))
                #t)

  (check-equal? (t=? c (t* b (tslice a '() #:skip-dim 2)))
                #f)

  (check-equal? (in-tensor (t+ a b))
                #(2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0 14.0 15.0 
                  16.0 17.0 18.0 19.0 20.0 21.0 22.0 23.0 24.0 25.0 26.0 27.0 28.0))
  (check-equal? (in-tensor (t+ a (tslice a '() #:skip-dim 0)))
                #(0.0 2.0 4.0 6.0 8.0 10.0 12.0 14.0 16.0
                  9.0 11.0 13.0 15.0 17.0 19.0 21.0 23.0 25.0
                  18.0 20.0 22.0 24.0 26.0 28.0 30.0 32.0 34.0))
  (check-equal? (in-tensor (t+ (tslice a '() #:skip-dim 0) a))
                #(0.0 2.0 4.0 6.0 8.0 10.0 12.0 14.0 16.0
                  9.0 11.0 13.0 15.0 17.0 19.0 21.0 23.0 25.0
                  18.0 20.0 22.0 24.0 26.0 28.0 30.0 32.0 34.0))
  (check-equal? (in-tensor (t+ (tslice a '() #:skip-dim 0) (tslice a '() #:skip-dim 0)))
                #(0.0 2.0 4.0 6.0 8.0 10.0 12.0 14.0 16.0))
  (check-exn exn:fail? (thunk (t+ a (make-tensor (vector 2) 1.0 #:ctype _double))))

  )

(define (test-matmul)
  (define X (make-tensor (vector 8 4)
                         (vector 1.0 1.0 2.0 1.0
                                 2.0 1.0 2.0 1.0
                                 2.0 2.0 2.0 1.0
                                 1.0 2.0 2.0 1.0
                                 1.0 1.0 1.0 1.0
                                 2.0 1.0 1.0 1.0
                                 2.0 2.0 1.0 1.0
                                 1.0 2.0 1.0 1.0)
                         #:ctype _double))
  (define T (make-tensor (vector 4 4)
                         (vector 1.0  0.0    0.0   0.0
                                 0.0  0.866  0.5   0.0
                                 0.0 -0.5    0.866 0.0
                                 0.0  0.951 -0.549 1.0)))
  (define result (make-tensor (vector 8 4)
                              (vector 1.0 0.817 1.683 1.0
                                      2.0 0.817 1.683 1.0
                                      2.0 1.683 2.183 1.0
                                      1.0 1.683 2.183 1.0
                                      1.0 1.317 0.817 1.0
                                      2.0 1.317 0.817 1.0
                                      2.0 2.183 1.317 1.0
                                      1.0 2.183 1.317 1.0)))

  (check-within (in-tensor (t** X T))
                (in-tensor result)
                0.01)

  ; check slicing with empty slices
  (check-equal? (t=? result (tslice result '()))
                #t)
  
  (check-equal? (t=? result (tslice result '(() ())))
                #t)

  (check-equal? (in-tensor (tslice result '() #:skip-dim 0))
                #(1.0 0.817 1.683 1.0))

  (check-equal? (in-tensor (tslice result '(()) #:skip-dim 0))
                #(1.0 0.817 1.683 1.0))

  (check-equal? (in-tensor (tslice result '() #:skip-dim 1))
                #(1.0 2.0 2.0 1.0 1.0 2.0 2.0 1.0))

  (check-equal? (in-tensor (tslice result '(()) #:skip-dim 1))
                #(1.0 2.0 2.0 1.0 1.0 2.0 2.0 1.0))
  
  ; slice a single column
  (check-equal? (in-tensor (tslice result '((0 7 1) (1 1 1))))
                #(0.817 0.817 1.683 1.683 1.317 1.317 2.183 2.183))

  (check-equal? (in-tensor (tslice result '(() (1 1 1))))
                #(0.817 0.817 1.683 1.683 1.317 1.317 2.183 2.183))
  
  ;; invalid slice values
  (check-exn exn:fail? (thunk (tslice result '((0 7 1) (1 1 0)))))
  (check-exn exn:fail? (thunk (tslice result '((0 8 1) (1 1 1)))))
  (check-exn exn:fail? (thunk (tslice result '((-1 7 1) (1 1 1)))))
  )

(define (test-geom)
  (define row-vectors (make-tensor (vector 2 4) (vector 1.0 0.0 1.0 1.0
                                                        1.0 5.0 1.0 1.0)))
  (define a (t** row-vectors (tensor-rotate-z-row-3d 90)))
  (check-within (in-tensor a)
                #(0.0  1.0 1.0 1.0
                  -5.0 1.0 1.0 1.0)
                0.0001)
  )

(define (sum-columns size)
  (define a (make-tensor (vector size size) 'index))
  (define results (make-vector size))
  (for ([i (in-range 0 size)])
    (vector-set! results
                 i
                 (tsum (tslice a `(() (,i ,i 1))))))
  results)

;; Can now parallelize in Racket 8.18 with added #:merely-uninterruptible? keyword in allocator wrapper
;; (needed because tslice calls ndarray_iter_new) 
(define (sum-columns-parallel size)
  (define a (make-tensor (vector size size) 'index))
  (define num-threads (processor-count))
  (define batch-size (/ size num-threads))
  (define results (make-vector size))
  (define fs
    (for/list ([i (in-range num-threads)])
      (future (lambda ()
                (for ([j (in-range (* i batch-size) (* (add1 i) batch-size))])
                  (vector-set! results
                               j
                               (tsum (tslice a `(() (,j ,j 1))))))))))
  (for ([f (in-list fs)])
    (touch f))
  results)

;; uses post-racket 8.18 parallel threads
(define (sum-columns-parallel-threads size)
  (define a (make-tensor (vector size size) 'index))
  (define num-threads (processor-count))
  (define batch-size (/ size num-threads))
  (define results (make-vector size))
  (define thds
    (for/list ([i (in-range num-threads)])
      (thread (lambda ()
                (for ([j (in-range (* i batch-size) (* (add1 i) batch-size))])
                  (vector-set! results
                               j
                               (tsum (tslice a `(() (,j ,j 1)))))))
              #:pool 'own)))
  (for ([thd (in-list thds)])
    (thread-wait thd))
  results)

(define (time-sum-columns size)
  (define (cleanup)
    (collect-garbage)
    (collect-garbage)
    (collect-garbage))
  (define a (make-tensor (vector size size) 'index))
  (define num-threads (processor-count))
  (define batch-size (/ size num-threads))
  (define results (make-vector size))
  (cleanup)
  ;; serial version
  (time ((thunk
         (for ([i (in-range 0 size)])
           (vector-set! results
                        i
                        (tsum (tslice a `(() (,i ,i 1)))))))))
  ;; future version
  (cleanup)
  (time ((thunk
         (define fs
           (for/list ([i (in-range num-threads)])
             (future (lambda ()
                       (for ([j (in-range (* i batch-size) (* (add1 i) batch-size))])
                  (vector-set! results
                               j
                               (tsum (tslice a `(() (,j ,j 1))))))))))
         (for ([f (in-list fs)])
           (touch f)))))
  ;; parallel thread version
  (cleanup)
  (time ((thunk
         (define thds
           (for/list ([i (in-range num-threads)])
             (thread (lambda ()
                       (for ([j (in-range (* i batch-size) (* (add1 i) batch-size))])
                         (vector-set! results
                                      j
                                      (tsum (tslice a `(() (,j ,j 1)))))))
                     #:pool 'own)))
         (for ([thd (in-list thds)])
           (thread-wait thd))))))
  
(define (sum-tensor-parallel size)
  (define a (make-tensor (vector size size) 'index))
  (define num-threads (processor-count))
  (define batch-size (/ size num-threads))
  (define fs
    (for/list ([i (in-range num-threads)])
      (future (lambda ()
                (for ([j (in-range (* i batch-size) (* (add1 i) batch-size))])
                  (tsum a))))))
  (for ([f (in-list fs)])
    (touch f)))

;; uses post-racket 8.18 parallel threads
(define (sum-tensor-parallel-threads size)
  (define a (make-tensor (vector size size) 'index))
  (define num-threads (processor-count))
  (define batch-size (/ size num-threads))
  (define thds
    (for/list ([i (in-range num-threads)])
      (thread (lambda ()
                (for ([j (in-range (* i batch-size) (* (add1 i) batch-size))])
                  (tsum a)))
              #:pool 'own)))
  (for ([thd (in-list thds)])
    (thread-wait thd)))

(define (sum-rows size)
  (define a (make-tensor (vector size size) 'index))
  (for ([i (in-range 0 size)])
    (tsum (tslice a `((,i ,i 1) ,empty)))))

(define (test-complex)
  (define a (make-tensor (vector 2 2) (make-flrectangular 4.0 1.42) #:ctype _complex))
  
  (tset! a 1 0 (sqrt -2.0))
  (check-equal? (tref a 1 1) (make-flrectangular 4.0 1.42))
  (check-equal? (tref a 1 0) (sqrt -2.0))

  (define b (make-tensor (vector 2 2) (make-flrectangular 2.0 3.0) #:ctype _complex))
  (define c (make-tensor (vector 2 2) (make-flrectangular 4.0 6.0) #:ctype _complex))
  (define result (t* b c))
  (check-within (tref result 0 0) -10+24i 0.0001)
  (check-within (tref result 0 1) -10.0+24.0i 0.0001)
  (check-within (tref result 1 0) (make-flrectangular -10.0 24.0) 0.0001)
  (check-within (tref result 1 1) (make-rectangular -10.0 24.0) 0.0001))

(define (test-basic)
  (define a (make-tensor (vector 10 10 3) 1.0))
  (define it1 (tslice a '((0 4 1) () ())))
  (check-equal? (tlen a) 300)
  (check-equal? (tlen a 0) 30)
  (check-equal? (tlen a 1) 3)
  (check-equal? (tlen it1) 150)
  (check-equal? (tlen it1 0) 30)
  (check-equal? (tlen it1 1) 3)
  (check-equal? (tref a 150) 1.0)
  (check-exn exn:fail? (thunk (tref a 300)))
  (check-exn exn:fail? (thunk (tref it1 150)))
  (tset! a 299 2.0)
  (tset! a 149 3.0)
  (check-exn exn:fail? (thunk (tset! it1 150 1.0)))
  (check-equal? (tref a 299) 2.0)
  (check-equal? (tref a 149) 3.0))
  
(define (test-sum)
  (define x (make-tensor (vector 5 3)
                         (vector 8.54  1.54  8.12
                                 3.13  8.76  5.29
                                 7.73  6.71  1.31
                                 6.44  9.64  8.44
                                 7.27  8.42  5.2)
                         #:ctype _double))
  void)

(define (run-tests)
  (test-basic)
  (test-linspace)
  (test-ppm)
  (test-tmap)
  (test-build-tensor)
  (test-ops)
  (test-matmul)
  (test-geom)
  (test-complex))

(run-tests)
