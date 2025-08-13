#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt"
         "tensor.rkt")

(provide tensor-scale-3d
         tensor-translate-3d
         tensor-rotate-x-3d
         tensor-rotate-y-3d
         tensor-rotate-z-3d
         tensor-ortho-proj-3d
         tensor-persp-proj-3d
         tensor-ray-transform-3d
         tensor-translate-row-3d
         tensor-rotate-x-row-3d
         tensor-rotate-y-row-3d
         tensor-rotate-z-row-3d
         tensor-ortho-proj-row-3d
         tensor-persp-proj-row-3d
         tensor-ray-transform-row-3d)

(define (radians->degress x)
  (* (/ x 180) pi))

;; create a homogeneous transformation matrix to scale an array of vectors by x y z
;;
;; ctype must be either _double or _float
(define (tensor-scale-3d x y z #:ctype [ctype _double])
  (make-tensor (vector 4 4)
               (vector   x 0.0 0.0 0.0
                       0.0   y 0.0 0.0
                       0.0 0.0   z 0.0
                       0.0 0.0 0.0 1.0)
               #:ctype ctype))

;; create a homogeneous transformation matrix to translate an array of column vectors by x y z
;;
;; ctype must be either _double or _float
(define (tensor-translate-3d x y z #:ctype [ctype _double])
    (make-tensor (vector 4 4)
               (vector 1.0 0.0 0.0   x
                       0.0 1.0 0.0   y
                       0.0 0.0 1.0   z
                       0.0 0.0 0.0 1.0)
               #:ctype ctype))

;; create a homogeneous transformation matrix to rotate an array of column vectors about the x axis
;;
;; ctype must be either _double or _float
(define (tensor-rotate-x-3d deg #:ctype [ctype _double])
  (define theta (radians->degress deg))
  
  (make-tensor (vector 4 4)
               (vector 1.0         0.0              0.0  0.0
                       0.0 (cos theta)  (- (sin theta))  0.0
                       0.0 (sin theta)      (cos theta)  0.0
                       0.0         0.0              0.0  1.0)
               #:ctype ctype))

;; create a homogeneous transformation matrix to rotate an array of column vectors about the y axis
;;
;; ctype must be either _double or _float
(define (tensor-rotate-y-3d deg #:ctype [ctype _double])
  (define theta (radians->degress deg))
  
  (make-tensor (vector 4 4)
               (vector     (cos theta)  0.0  (sin theta)  0.0
                                   0.0  1.0          0.0  0.0
                       (- (sin theta))  0.0  (cos theta)  0.0
                                   0.0  0.0          0.0  1.0)
               #:ctype ctype))

;; create a homogeneous transformation matrix to rotate an array of column vectors about the z axis
;;
;; ctype must be either _double or _float
(define (tensor-rotate-z-3d deg #:ctype [ctype _double])
  (define theta (radians->degress deg))
  
  (make-tensor (vector 4 4)
               (vector (cos theta)  (- (sin theta))  0.0 0.0
                       (sin theta)      (cos theta)  0.0 0.0
                               0.0             0.0   1.0 0.0
                               0.0             0.0   0.0 1.0)
               #:ctype ctype))

(define (tensor-rotate-axis-3d deg x y z #:ctype [ctype _double])
  (define theta (radians->degress deg))
  void)

(define (tensor-ortho-proj-3d r l t b f n #:ctype [ctype _double])
  (define tx (/ (+ r l) (- r l)))
  (define ty (/ (+ t b) (- t b)))
  (define tz (- (/ (+ f n) (- f n))))
  (make-tensor (vector 4 4)
               (vector (/ 2.0 (- r l))              0.0              0.0    tx
                                   0.0  (/ 2.0 (- t b))              0.0    ty
                                   0.0              0.0  (/ -2.0 (- f n))   tz
                                   0.0              0.0              0.0   1.0)
               #:ctype ctype))

(define (tensor-persp-proj-3d r l t b f n #:ctype [ctype _double])
  (define A (/ (+ r l) (- r l)))
  (define B (/ (+ t b) (- t b)))
  (define C (- (/ (+ f n) (- f n))))
  (define D (- (/ (* 2.0 f n) (- f n))))
  (make-tensor (vector 4 4)
               (vector (/ (* 2.0 n) (- r l))                     0.0    A    0.0
                                         0.0   (/ (* 2.0 n) (- t b))    B    0.0
                                         0.0                     0.0    C      D
                                         0.0                     0.0 -1.0    0.0)
               #:ctype ctype))

;;
(define (tensor-ray-transform-3d x y d #:ctype [ctype _double])
  (make-tensor (vector 4 4)
               (vector     1.0 0.0 (/ x d) 0.0
                           0.0 1.0 (/ y d) 0.0
                           0.0 0.0     1.0 0.0
                           0.0 0.0     0.0 1.0)
               #:ctype ctype))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These are row vector versions of the transformation matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create a homogeneous transformation matrix to translate an array of row vectors by x y z
;;
;; ctype must be either _double or _float
(define (tensor-translate-row-3d x y z #:ctype [ctype _double])
    (make-tensor (vector 4 4)
               (vector 1.0 0.0 0.0 0.0
                       0.0 1.0 0.0 0.0
                       0.0 0.0 1.0 0.0
                         x   y   z 1.0)
               #:ctype ctype))

;; create a homogeneous transformation matrix to rotate an array of row vectors about the x axis
;;
;; ctype must be either _double or _float
(define (tensor-rotate-x-row-3d deg #:ctype [ctype _double])
  (define theta (radians->degress deg))
  
  (make-tensor (vector 4 4)
               (vector 1.0 0.0             0.0         0.0
                       0.0 (cos theta)     (sin theta) 0.0
                       0.0 (- (sin theta)) (cos theta) 0.0
                       0.0 0.0             0.0         1.0)
               #:ctype ctype))

;; create a homogeneous transformation matrix to rotate an array of row vectors about the y axis
;;
;; ctype must be either _double or _float
(define (tensor-rotate-y-row-3d deg #:ctype [ctype _double])
  (define theta (radians->degress deg))
  
  (make-tensor (vector 4 4)
               (vector (cos theta) 0.0  (- (sin theta)) 0.0
                       0.0         1.0  0.0             0.0
                       (sin theta) 0.0  (cos theta)     0.0
                       0.0         0.0  0.0             1.0)
               #:ctype ctype))

;; create a homogeneous transformation matrix to rotate an array of row vectors about the z axis
;;
;; ctype must be either _double or _float
(define (tensor-rotate-z-row-3d deg #:ctype [ctype _double])
  (define theta (radians->degress deg))
  
  (make-tensor (vector 4 4)
               (vector (cos theta)     (sin theta)  0.0 0.0
                       (- (sin theta)) (cos theta)  0.0 0.0
                       0.0             0.0          1.0 0.0
                       0.0             0.0          0.0 1.0)
               #:ctype ctype))

(define (tensor-rotate-axis-row-3d deg x y z #:ctype [ctype _double])
  (define theta (radians->degress deg))
  void)

(define (tensor-ortho-proj-row-3d r l t b f n #:ctype [ctype _double])
  (define tx (/ (+ r l) (- r l)))
  (define ty (/ (+ t b) (- t b)))
  (define tz (- (/ (+ f n) (- f n))))
  (make-tensor (vector 4 4)
               (vector (/ 2.0 (- r l))              0.0              0.0   0.0
                                   0.0  (/ 2.0 (- t b))              0.0   0.0
                                   0.0              0.0  (/ -2.0 (- f n))  0.0
                                     t               ty                tz  1.0)
               #:ctype ctype))

(define (tensor-persp-proj-row-3d r l t b f n #:ctype [ctype _double])
  (define A (/ (+ r l) (- r l)))
  (define B (/ (+ t b) (- t b)))
  (define C (- (/ (+ f n) (- f n))))
  (define D (- (/ (* 2.0 f n) (- f n))))
  (make-tensor (vector 4 4)
               (vector (/ (* 2.0 n) (- r l))                     0.0  0.0    0.0
                                         0.0   (/ (* 2.0 n) (- t b))  0.0    0.0
                                           A                       B    C   -1.0
                                         0.0                     0.0    D    0.0)
               #:ctype ctype))

;;
(define (tensor-ray-transform-row-3d x y d #:ctype [ctype _double])
  (make-tensor (vector 4 4)
               (vector     1.0     0.0 0.0 0.0
                           0.0     1.0 0.0 0.0
                       (/ x d) (/ y d) 1.0 0.0
                           0.0     0.0 0.0 1.0)
               #:ctype ctype))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END row vector versions of the transformation matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
