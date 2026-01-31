#lang racket

(require racket/flonum
         ffi/unsafe
         "tensor.rkt")

(define x (make-tensor (vector 5 3)
                       (vector 8.54  1.54  8.12
                               3.13  8.76  5.29
                               7.73  6.71  1.31
                               6.44  9.64  8.44
                               7.27  8.42  5.27)
                       #:ctype _double))

(define y (make-tensor (vector 6 3)
                       (vector 8.65  0.27  4.67
                               7.73  7.26  1.95
                               1.27  7.27  3.59
                               4.05  5.16  3.53
                               4.77  6.48  8.01
                               7.85  6.68  6.13)
                       #:ctype _double))

(define (pairwise-dist-unoptimized x y)
  (define diffs (t- (tslice x '() #:add-dim 1)
                    (tslice y '() #:add-dim 0)))
  (print-tensor diffs)
  (display (tshape diffs))
  (define dists (tsqrt (tsum (texpt diffs 2.0) #:axis 1)))
  (print-tensor dists)
  (display (tshape dists))
  dists)

(define (pairwise-dist x y)
  (define xsquared (texpt x 2))
  (define ysquared (texpt y 2.0))
  (print-tensor xsquared)
  (print-tensor ysquared)
  )
