#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum
         racket/runtime-path)

(require rackunit)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt"
         "ndarray-matrix-ffi.rkt"
         "ndarray-io-ffi.rkt")

#|
(define _shape-array-1d (_array _intptr 1))
(define shape (malloc _shape-array-1d 'atomic))
(array-set! (ptr-ref shape _shape-array-1d 0) 0 10) ; `ptr-ref` instead of `cast`
(array-ref (ptr-ref shape _shape-array-1d 0) 0)
|#

(define (print-iter2d it type)
  (define row-len (ndarray-dims (NDArrayIter-nda it) 1))
  (for ([x (in-iter it type)]
        [i (in-naturals 0)])
    (when (and (= (remainder i row-len) 0)
               (not (zero? i)))
      (printf "~n"))
    (printf " ~a" x))
  (printf "~n"))

(define (print-iter it type)
  (cond
    [(equal? (NDArrayIter-nd_m1 it) 1)
     (print-iter2d it type)]
    [else
     (for ([x (in-iter it type)]
           [i (in-naturals 0)])
       (printf "it[~a] = ~a~n" i x))]))

(define (print-array nda type)
  (define it (ndarray_iter_new nda #f))
  (print-iter it type))

(define (test-ndarray)
  (define a (ndarray_new 2 (vector 4 4) (ctype-sizeof _uint16) #f))
  (define b (ndarray_new 2 (vector 4 4) (ctype-sizeof _uint16) #f))
  (define a_ref (ndarray_new 2 (vector 4 4) (ctype-sizeof _uint16) (NDArray-dataptr a)))
  (ndarray_fill_uint16_t a 42)
  (ndarray_fill_uint16_t b 0)
  (check-equal? (ndarray_shape_equal a b) #t)
  (check-equal? (ndarray_data_equal a b) #f)
  (check-equal? (ndarray_data_equal a a_ref) #t)
  
  (define a_copy (ndarray_copy a))
  (check-equal? (ndarray_data_equal a a_copy) #t)

  (define c (ndarray_new 2 (vector 4 5) (ctype-sizeof _uint16) #f))
  (ndarray_fill_uint16_t c 42)
  (check-equal? (ndarray_shape_equal a c) #f)
  (check-equal? (ndarray_data_equal a c) #f)

  (define e (ndarray_new 2 (vector 4 1) (ctype-sizeof _int32) #f))
  (define f (ndarray_new 2 (vector 1 4) (ctype-sizeof _int32) #f))
  (ndarray_fill_index_int32_t e)
  (ndarray_fill_index_int32_t f)
  (check-equal? (ndarray_equal e f) #f)
  (check-equal? (ndarray_data_equal e f) #t))

(define (test-simple-iterator)
  (define nda (ndarray_new 2 (vector 10 5) (ctype-sizeof _int) #f))
  (define it (ndarray_iter_new nda #f))
  (let loop ([cursor (NDArrayIter-cursor it)]
             [i 0])
    (ptr-set! cursor _int 0 i)
    (when (ndarray_iter_next it)
      (loop (NDArrayIter-cursor it) (add1 i))))
  (ndarray_iter_reset it)  
  ;(print-iter it _int)
  (check-equal?
   (ndarray-data->list nda _int)
   '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49))

  (define it2 (ndarray_iter_new nda (make-slice 2 '((0 8 2) (1 3 1)))))
  ;(print-iter it2 _int)
  (check-equal? (for/list ([x (in-iter it2 _int)])
                  x)
                '(1 2 3 11 12 13 21 22 23 31 32 33 41 42 43)))

(define (test-multi-iterator)
  (define A (flvector 0.0   0.0  0.0
                      10.0 10.0 10.0
                      20.0 20.0 20.0
                      30.0 30.0 30.0))
  (define B (flvector 1.0 2.0 3.0))
  (define nda-a (ndarray_new 2 (vector 4 3) (ctype-sizeof _double) (flvector->cpointer A)))
  (define nda-b (ndarray_new 1 (vector 3) (ctype-sizeof _double) (flvector->cpointer B)))
  (define it-a (ndarray_iter_new nda-a #f))
  (define it-b (ndarray_iter_new nda-b #f))
  #|
  (printf "Multi Iteration Tests~n")
  (printf "---------------------~n")
  (printf "A~n")
  (print-iter it-a _double)
  (printf "B~n")
  (print-iter it-b _double)
  |#
  (check-equal? (for/list ([x (in-iter it-a _double)])
                  x)
                '(0.0 0.0 0.0 10.0 10.0 10.0 20.0 20.0 20.0 30.0 30.0 30.0))
  (check-equal? (for/list ([x (in-iter it-b _double)])
                  x)
                '(1.0 2.0 3.0))
  
  ;(printf "A*B~n")
  ;(print-iter (ndarray_iter_new (ndarray_mul_double nda-a nda-b) #f) _double)
  (check-equal? (for/list ([x (in-iter (ndarray_iter_new (ndarray_mul_double nda-a nda-b) #f) _double)])
                  x)
                '(0.0 0.0 0.0 10.0 20.0 30.0 20.0 40.0 60.0 30.0 60.0 90.0))
  
  (define nda-d (ndarray_mul_double nda-a (ndarray_new 1 (vector 1) (ctype-sizeof _double) (flvector->cpointer (flvector 1.5)))))
  ;(printf "A*1.5~n")
  ;(print-iter (ndarray_iter_new nda-d #f) _double)
  (check-equal? (for/list ([x (in-iter (ndarray_iter_new nda-d #f) _double)])
                  x)
                '(0.0 0.0 0.0 15.0 15.0 15.0 30.0 30.0 30.0 45.0 45.0 45.0))
  
  ;(printf "A*10~n")
  ;(print-iter (ndarray_iter_new (ndarray_iter_mul_double it-a (ndarray_iter_new nda-a (make-slice 2 '((1 1 1) (0 0 1))))) #f) _double)
  (check-equal? (for/list ([x (in-iter (ndarray_iter_new (ndarray_iter_mul_double
                                                          it-a
                                                          (ndarray_iter_new nda-a (make-slice 2 '((1 1 1) (0 0 1))))) #f)
                                       _double)])
                  x)
                '(0.0 0.0 0.0 100.0 100.0 100.0 200.0 200.0 200.0 300.0 300.0 300.0))
  
  (define nda-3d (ndarray_new 3 (vector 2 2 2) (ctype-sizeof _double) #f))
  (define nda-2d (ndarray_new 2 (vector 2 2) (ctype-sizeof _double) #f))
  (ndarray_fill_double nda-3d 10.0)
  (ndarray_fill_index_double nda-2d)
  ;(printf "2x2x2 matrix * 2x2~n")
  ;(print-iter (ndarray_iter_new (ndarray_mul_double nda-3d nda-2d) #f) _double)
  (check-equal? (for/list ([x (in-iter (ndarray_iter_new (ndarray_mul_double nda-3d nda-2d) #f) _double)])
                  x)
                '(0.0 10.0 20.0 30.0 0.0 10.0 20.0 30.0)))

(define (linspace start stop [num 50] [endpoint? #t])
  (define num-points (if endpoint? (add1 num) num))
  (define shape (vector num-points))
  (define indexes (ndarray_new 1 shape (ctype-sizeof _double) #f))
  (ndarray_fill_index_double indexes)

  (define scalar_shape (vector 1))
  (define start-data (cvector _double (->fl start)))
  (define stop-data (cvector _double (->fl (- stop start))))
  (define step-data  (cvector _double (exact->inexact (/ 1 num))))
  
  (define a-start (ndarray_new 1 scalar_shape (ctype-sizeof _double) (cvector-ptr start-data)))
  (define a-span (ndarray_new 1 scalar_shape (ctype-sizeof _double) (cvector-ptr stop-data)))
  (define a-step (ndarray_new 1 scalar_shape (ctype-sizeof _double) (cvector-ptr step-data)))

  ;(define result (ndarray_mul_double indexes a-step))
  #;(list
   (map (lambda (i) (ndarray-ref indexes _double i)) '(0 1 2 3 4))
   (ndarray-ref a-start _double 0)
   (ndarray-ref a-span _double 0)
   (ndarray-ref a-step _double 0)
   (map (lambda (i) (ndarray-ref result _double i)) '(0 1 2 3 4))
   ;(map (lambda (i) (ptr-ref (NDArray-dataptr (ptr-ref result _NDArray)) _double i)) '(0 1 2 3 4))
   (ndarray-dims result 0)
   (NDArray-elem_bytes (ptr-ref result _NDArray)))
   
  (ndarray_add_double a-start (ndarray_mul_double a-span (ndarray_mul_double a-step indexes))))

(define (test-linspace)
  (define result (linspace 0 5000 50 #t))
  (check-equal?
   (map (lambda (i) (ndarray-ref result _double i)) '(0 1 2 3 4 5 50))
   '(0.0 100.0 200.0 300.0 400.0 500.0 5000.0)))

(define-runtime-path test-ppm-path "../test/image-01.ppm")
(define-runtime-paths (out-ppm-path out2-ppm-path out3-ppm-path)
  (values "out.ppm" "out2.ppm" "out3.ppm"))

(define (test-ppm)
  (define-values (rgb w h) (read_ppm test-ppm-path))
  (define nda (ndarray_new 3 (vector w h 3) 1 rgb))
  (ndarray_iter_write_ppm (ndarray_iter_new nda #f) out-ppm-path w h)

  (define skip_dim (malloc _int))
  (ptr-set! skip_dim _int 0 -1)
  ;; writing a ppm with skip dim set is no longer supported by ndarray_iter_write_file
  ;(define it2 (ndarray_iter_new_all_but_axis nda #f skip_dim))
  ;(ndarray_iter_write_ppm it2 out2-ppm-path w h)

  (ndarray_iter_write_ppm (ndarray_iter_new nda (make-slice 3 `((0 ,(sub1 h)  2)
                                                                (0 ,(sub1 w) 2)
                                                                (0 2 1))))
                          out3-ppm-path (/ w 2) (/ h 2))
  (free rgb))

;; check that exceptions are triggered for certain conditions
(define (test-err)
  (define a (ndarray_new 2 (vector 3 3) (ctype-sizeof _int32) #f))
  (define b (ndarray_new 2 (vector 3 4) (ctype-sizeof _int32) #f))
  (ndarray_fill_index_int32_t a)
  (ndarray_fill_index_int32_t b)
  (check-equal?
   (with-handlers ([exn:fail? (lambda (exn) "caught error")])
     (ndarray_add_int32_t a b))
   "caught error")
  (check-equal?
   (with-handlers ([exn:fail? (lambda (exn) "caught error")])
     (ndarray_mul_int32_t a b))
   "caught error"))

(define (test-ops)
  (define a (ndarray_new 2 (vector 10 10) (ctype-sizeof _double) #f))
  (ndarray_fill_index_double a)
  (define b (ndarray_new 2 (vector 10 10) (ctype-sizeof _float) #f))
  (ndarray_fill_index_float b)

  (define skip_dim (malloc _int))
  (ptr-set! skip_dim _int 0 -1)

  (check-equal? (ndarray_sum_double a) 4950.0)
  (check-equal? (ndarray_sum_float b) 4950.0)
  (check-equal? (ndarray_iter_sum_double (ndarray_iter_new a (make-slice 2 '((0 2 1) (0 2 1))))) 99.0)
  (check-equal? (ndarray_iter_sum_float (ndarray_iter_new_all_but_axis b #f skip_dim)) 450.0)
  
  (define c (ndarray_expt_double a 2.0))
  (check-equal? (ndarray-ref c _double 0 0) 0.0)
  (check-equal? (ndarray-ref c _double 0 1) 1.0)
  (check-equal? (ndarray-ref c _double 0 2) 4.0)
  (check-equal? (ndarray-ref c _double 5 0) 2500.0)
  (check-equal? (ndarray-ref c _double 9 9) 9801.0)

  (define d (ndarray_expt_float b 2.0))
  (check-equal? (ndarray-ref d _float 0 0) 0.0)
  (check-equal? (ndarray-ref d _float 0 1) 1.0)
  (check-equal? (ndarray-ref d _float 0 2) 4.0)
  (check-equal? (ndarray-ref d _float 5 0) 2500.0)
  (check-equal? (ndarray-ref d _float 9 9) 9801.0)

  (define e (ndarray_iter_expt_float (ndarray_iter_new b #f) 2.0))
  (check-equal? (ndarray-ref e _float 5 0) 2500.0))

(define (test-bigmul)
  (define a (ndarray_new 2 (vector 10000 10000) 8 #f))
  (define b (ndarray_new 2 (vector 10000 10000) 8 #f))
  (ndarray_fill_index_double a)
  (ndarray_fill_double b 2.0)
  (define c (ndarray_mul_double a b))
  (check-equal? (ndarray-ref c _double 1 0) 20000.0)
  (check-equal? (ndarray-ref c _double 0 1000) 2000.0)
  (check-equal? (ndarray-ref c _double 1000 1000) 20002000.0)
  (check-equal? (ndarray-ref c _double 9999 9999) 199999998.0))

(define (test-bigadd)
  (define a (ndarray_new 2 (vector 10000 10000) 4 #f))
  (define b (ndarray_new 2 (vector 10000 10000) 4 #f))
  (ndarray_fill_float a 8.0)
  (ndarray_fill_float b 2.0)
  (define c (ndarray_add_float a b))
  (check-equal? (ndarray-ref c _float 1 0) 10.0)
  (check-equal? (ndarray-ref c _float 0 1000) 10.0)
  (check-equal? (ndarray-ref c _float 1000 1000) 10.0)
  (check-equal? (ndarray-ref c _float 9999 9999) 10.0))

(define (test-bigsum)
  (define a (ndarray_new 2 (vector 10000 10000) 8 #f))
  (ndarray_fill_index_double a)
  (check-equal? (ndarray_sum_double a) 4.99999995e+15))

(define (time-mul-double x)
  (define a (ndarray_new 2 (vector x x) 8 #f))
  (define b (ndarray_new 2 (vector x x) 8 #f))
  (ndarray_fill_index_double a)
  (ndarray_fill_double b 2.0)
  (time
   (ndarray_mul_double a b)))

(define (time-sum-double x)
  (define a (ndarray_new 2 (vector x x) 8 #f))
  (ndarray_fill_index_double a)
  (time
   (ndarray_sum_double a)))

;; now fixed
(define (iter-crash)
  (define nda (ndarray_new 1 (vector 256) 4 #f))
  (ndarray_fill_index_int32_t nda)
  (print-iter (ndarray_iter_new nda #f) _int32))

(define (test-equal)
  (define a (ndarray_new 3 (vector 4 4 4) 2 #f))
  (define b (ndarray_new 2 (vector 4 4) 2 #f))
  (ndarray_fill_int16_t a 2)
  (ndarray_fill_int16_t b 2)

  (check-equal? (ndarray_equal a b) #f)
  (check-equal? (ndarray_iter_equal_int16_t (ndarray_iter_new a #f)
                                            (ndarray_iter_new b #f))
                #t)

  (define c (ndarray_new 2 (vector 4 3) 2 #f))
  (ndarray_fill_int16_t c 2)
  (check-equal? (ndarray_iter_equal_int16_t (ndarray_iter_new b #f)
                                            (ndarray_iter_new c #f))
                #f)
  
  void)
  
(define (test-matmul)
  (define a (ndarray_new 2 (vector 2 2) 8 #f))
  (define b (ndarray_new 2 (vector 2 2) 8 #f))
  (ndarray_fill_double a 2.0)
  (ndarray_fill_mat_ident_double b)

  (check-equal? (ndarray-ref b _double 0 0) 1.0)
  (check-equal? (ndarray-ref b _double 0 1) 0.0)
  (check-equal? (ndarray-ref b _double 1 1) 1.0)
  (check-equal? (ndarray-ref b _double 1 0) 0.0)

  (define c (ndarray_matmul_double a b))

  (check-equal? (ndarray-ref c _double 0 0) 2.0)
  (check-equal? (ndarray-ref c _double 0 1) 2.0)
  (check-equal? (ndarray-ref c _double 1 1) 2.0)
  (check-equal? (ndarray-ref c _double 1 0) 2.0)

  (define u (ndarray_new 2 (vector 32 32) 8 #f))
  (define v (ndarray_new 2 (vector 32 32) 8 #f))
  (ndarray_fill_index_double u)
  (ndarray_fill_mat_ident_double v)
  (define w (ndarray_matmul_double u v))

  (check-equal? (ndarray-ref w _double 0 0) 0.0)
  (check-equal? (ndarray-ref w _double 0 1) 1.0)
  (check-equal? (ndarray-ref w _double 1 0) 32.0)
  (check-equal? (ndarray_equal u w) #t)

  (define r (ndarray_new 2 (vector 2 3) 8 #f))
  (ndarray-set! r _int64 0 0 2)
  (ndarray-set! r _int64 0 1 3)
  (ndarray-set! r _int64 0 2 4)
  (ndarray-set! r _int64 1 0 1)
  (ndarray-set! r _int64 1 1 0)
  (ndarray-set! r _int64 1 2 0)
  ;(print-array r _int64)
  
  (define s (ndarray_new 2 (vector 3 2) 8 #f))
  (ndarray-set! s _int64 0 0 0)
  (ndarray-set! s _int64 0 1 1000)
  (ndarray-set! s _int64 1 0 1)
  (ndarray-set! s _int64 1 1 100)
  (ndarray-set! s _int64 2 0 0)
  (ndarray-set! s _int64 2 1 10)
  ;(print-array s _int64)
  
  (define t (ndarray_matmul_int64_t r s))
  ;(print-array t _int64)
  (check-equal? (ndarray-ref t _int64 0 0) 3)
  (check-equal? (ndarray-ref t _int64 0 1) 2340)
  (check-equal? (ndarray-ref t _int64 1 0) 0)
  (check-equal? (ndarray-ref t _int64 1 1) 1000)

  ; test iterator matmul
  (define tt (ndarray_iter_matmul_int64_t (ndarray_iter_new r #f) (ndarray_iter_new s #f)))
  ;(print-array tt _int64)
  (check-equal? (ndarray-ref tt _int64 0 0) 3)
  (check-equal? (ndarray-ref tt _int64 0 1) 2340)
  (check-equal? (ndarray-ref tt _int64 1 0) 0)
  (check-equal? (ndarray-ref tt _int64 1 1) 1000)
  
  ; error conditions
  (check-exn exn:fail? (thunk (ndarray_matmul_double a u)))
  (check-exn exn:fail? (thunk (ndarray_matmul_double (ndarray_new 3 (vector 3 3 3) 8 #f)
                                                     (ndarray_new 3 (vector 3 3 3) 8 #f))))
  (check-not-exn (thunk (ndarray_matmul_double (ndarray_new 2 (vector 1 32) 8 #f)
                                               (ndarray_new 2 (vector 32 1) 8 #f))))
  
  void)

(define (time-matmul x)
  (define a (ndarray_new 2 (vector x x) 8 #f))
  (define b (ndarray_new 2 (vector x x) 8 #f))
  (ndarray_fill_index_double a)
  (ndarray_fill_index_double b)
  (time
   (ndarray_matmul_double a b)))

(define (test-complex)
  (define a (ndarray_new 2 (vector 2 2) 16 #f))
  (ndarray_fill_complex a (make-rectangular 4.0 1.42))
  (ndarray-set! a _complex 1 0 (sqrt -2.0))
  (check-equal? (ndarray-ref a _complex 1 1) (make-flrectangular 4.0 1.42))
  (check-equal? (ndarray-ref a _complex 1 0) (sqrt -2.0)))

(define (test-utils)
  (define a (ndarray_new 3 (vector 7 6 3) 4 #f))
  (check-exn exn:fail? (thunk (ndarray-sub-elems a 3)))
  (check-equal? (ndarray-sub-elems a 2) 1)
  (check-equal? (ndarray-sub-elems a 1) 3)
  (check-equal? (ndarray-sub-elems a 0) 18)
  (define ita (ndarray_iter_new a #f))
  (check-exn exn:fail? (thunk (ndarray-iter-sub-elems ita 3)))
  (check-equal? (ndarray-iter-sub-elems ita 2) 1)
  (check-equal? (ndarray-iter-sub-elems ita 1) 3)
  (check-equal? (ndarray-iter-sub-elems ita 0) 18)
  (define ita2 (ndarray_iter_new a (make-slice 3 '((0 4 1) (0 4 2) (0 1 1)))))
  (check-exn exn:fail? (thunk (ndarray-iter-sub-elems ita2 3)))
  (check-equal? (ndarray-iter-sub-elems ita2 2) 1)
  (check-equal? (ndarray-iter-sub-elems ita2 1) 2)
  (check-equal? (ndarray-iter-sub-elems ita2 0) 6)
  (define skip_dim (malloc _int))
  (ptr-set! skip_dim _int 0 2)
  (define ita3 (ndarray_iter_new_all_but_axis a #f skip_dim))
  (check-exn exn:fail? (thunk (ndarray-iter-sub-elems ita3 3)))
  (check-equal? (ndarray-iter-sub-elems ita3 2) 1)
  (check-equal? (ndarray-iter-sub-elems ita3 1) 1)
  (check-equal? (ndarray-iter-sub-elems ita3 0) 6))
  
(define (run-tests)
  (test-linspace)
  (test-ndarray)
  (test-simple-iterator)
  (test-multi-iterator)
  (test-ppm)
  (test-ops)
  (test-err)
  (test-bigmul)
  (test-bigadd)
  (test-bigsum)
  (test-equal)
  (test-matmul)
  (test-complex)
  (test-utils)
  (collect-garbage 'major))

(run-tests)
