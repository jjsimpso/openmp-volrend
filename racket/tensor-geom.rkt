#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         racket/flonum)

(require "ndarray-ffi.rkt"
         "ndarray-ops-ffi.rkt"
         "ndarray-matrix-ffi.rkt"
         "tensor.rkt")

(provide tensor-identity-3d
         tensor-scale-3d
         tensor-translate-3d
         tensor-rotate-x-3d
         tensor-rotate-y-3d
         tensor-rotate-z-3d
         tensor-ortho-proj-3d
         tensor-persp-proj-3d
         tensor-ray-transform-3d
         tensor-inverse-transrot-3d
         tensor-inverse-3d
         tensor-translate-row-3d
         tensor-rotate-x-row-3d
         tensor-rotate-y-row-3d
         tensor-rotate-z-row-3d
         tensor-ortho-proj-row-3d
         tensor-persp-proj-row-3d
         tensor-ray-transform-row-3d
         tensor-lu-decomp
         tensor-mat-inverse)

(define (radians->degress x)
  (* (/ x 180) pi))

(define (dot3 u v)
  (fl+ (fl* (vector-ref u 0) (vector-ref v 0))
       (fl* (vector-ref u 1) (vector-ref v 1))
       (fl* (vector-ref u 2) (vector-ref v 2))))

(define (tensor-identity-3d #:ctype [ctype _double])
  (make-tensor (vector 4 4)
               (vector 1.0 0.0 0.0 0.0
                       0.0 1.0 0.0 0.0
                       0.0 0.0 1.0 0.0
                       0.0 0.0 0.0 1.0)
               #:ctype ctype))

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

;; find inverse of a 4x4 affine transformation matrix using shortcut:
;; [ inv(M)   -inv(M)*b ]
;; [   0           1    ]
;; M is 3x3 rotation part of matrix t
;; b is 3x1 translation part of matrix t
;; t must represent rotations + translation only
(define (tensor-inverse-transrot-3d t)
  (define type (tensor-type t))
  (define t-inv (make-tensor #(4 4) 0.0 #:ctype type))
  (define a (tensor-ndarray t))
  (define a-inv (tensor-ndarray t-inv))

  ;; 3x3 in upper left corner is just a transpose of t's upper left
  (ndarray-set! a-inv type 0 0 (ndarray-ref a type 0 0))
  (ndarray-set! a-inv type 1 0 (ndarray-ref a type 0 1))
  (ndarray-set! a-inv type 2 0 (ndarray-ref a type 0 2))
  
  (ndarray-set! a-inv type 0 1 (ndarray-ref a type 1 0))
  (ndarray-set! a-inv type 1 1 (ndarray-ref a type 1 1))
  (ndarray-set! a-inv type 2 1 (ndarray-ref a type 1 2))
  
  (ndarray-set! a-inv type 0 2 (ndarray-ref a type 2 0))
  (ndarray-set! a-inv type 1 2 (ndarray-ref a type 2 1))
  (ndarray-set! a-inv type 2 2 (ndarray-ref a type 2 2))

  ;; 3x1 in upper right is the inverse of 
  (define b (vector (ndarray-ref a type 0 3) (ndarray-ref a type 1 3) (ndarray-ref a type 2 3)))
  (ndarray-set! a-inv type 0 3 (dot3 (vector (fl* -1.0 (ndarray-ref a-inv type 0 0)) (fl* -1.0 (ndarray-ref a-inv type 0 1)) (fl* -1.0 (ndarray-ref a-inv type 0 2))) b))
  (ndarray-set! a-inv type 1 3 (dot3 (vector (fl* -1.0 (ndarray-ref a-inv type 1 0)) (fl* -1.0 (ndarray-ref a-inv type 1 1)) (fl* -1.0 (ndarray-ref a-inv type 1 2))) b))
  (ndarray-set! a-inv type 2 3 (dot3 (vector (fl* -1.0 (ndarray-ref a-inv type 2 0)) (fl* -1.0 (ndarray-ref a-inv type 2 1)) (fl* -1.0 (ndarray-ref a-inv type 2 2))) b))

  ;; bottom row is 0 0 0 1
  (ndarray-set! a-inv type 3 3 1.0)

  t-inv)

;; invert a 3x3 matrix using cofactors
(define (tensor-inverse-3x3 t)
  (define type (tensor-type t))
  (define a (tensor-ndarray t))

  (define adjA (make-tensor #(3 3) 0.0 #:ctype type))
  (define adjA-a (tensor-ndarray adjA))
  (ndarray-set! adjA-a type 0 0 (fl- (fl* (ndarray-ref a type 1 1) (ndarray-ref a type 2 2))
                                     (fl* (ndarray-ref a type 1 2) (ndarray-ref a type 2 1))))
  (ndarray-set! adjA-a type 0 1 (fl- (fl* (ndarray-ref a type 0 2) (ndarray-ref a type 2 1))
                                     (fl* (ndarray-ref a type 0 1) (ndarray-ref a type 2 2))))
  (ndarray-set! adjA-a type 0 2 (fl- (fl* (ndarray-ref a type 0 1) (ndarray-ref a type 1 2))
                                     (fl* (ndarray-ref a type 0 2) (ndarray-ref a type 1 1))))
  
  (ndarray-set! adjA-a type 1 0 (fl- (fl* (ndarray-ref a type 1 2) (ndarray-ref a type 2 0))
                                     (fl* (ndarray-ref a type 1 0) (ndarray-ref a type 2 2))))
  (ndarray-set! adjA-a type 1 1 (fl- (fl* (ndarray-ref a type 0 0) (ndarray-ref a type 2 2))
                                     (fl* (ndarray-ref a type 0 2) (ndarray-ref a type 2 0))))
  (ndarray-set! adjA-a type 1 2 (fl- (fl* (ndarray-ref a type 0 2) (ndarray-ref a type 1 0))
                                     (fl* (ndarray-ref a type 0 0) (ndarray-ref a type 1 2))))
  
  (ndarray-set! adjA-a type 2 0 (fl- (fl* (ndarray-ref a type 1 0) (ndarray-ref a type 2 1))
                                     (fl* (ndarray-ref a type 1 1) (ndarray-ref a type 2 0))))
  (ndarray-set! adjA-a type 2 1 (fl- (fl* (ndarray-ref a type 0 1) (ndarray-ref a type 2 0))
                                     (fl* (ndarray-ref a type 0 0) (ndarray-ref a type 2 1))))
  (ndarray-set! adjA-a type 2 2 (fl- (fl* (ndarray-ref a type 0 0) (ndarray-ref a type 1 1))
                                     (fl* (ndarray-ref a type 0 1) (ndarray-ref a type 1 0))))

  (define detA (+ (fl* (ndarray-ref a type 0 0) (fl- (fl* (ndarray-ref a type 1 1) (ndarray-ref a type 2 2))
                                                     (fl* (ndarray-ref a type 1 2) (ndarray-ref a type 2 1))))
                  (fl* (ndarray-ref a type 0 1) (fl- (fl* (ndarray-ref a type 1 2) (ndarray-ref a type 2 0))
                                                     (fl* (ndarray-ref a type 1 0) (ndarray-ref a type 2 2))))
                  (fl* (ndarray-ref a type 0 2) (fl- (fl* (ndarray-ref a type 1 0) (ndarray-ref a type 2 1))
                                                     (fl* (ndarray-ref a type 1 1) (ndarray-ref a type 2 0))))))

  (t* (/ 1.0 detA) adjA))

;; find inverse of a 4x4 affine transformation matrix using shortcut:
;; [ inv(M)   -inv(M)*b ]
;; [   0           1    ]
;; M is 3x3 transformation part of matrix t
;; b is 3x1 translation part of matrix t
(define (tensor-inverse-3d t)
  (define type (tensor-type t))
  (define t-inv (make-tensor #(4 4) 0.0 #:ctype type))
  (define a (tensor-ndarray t))
  (define a-inv (tensor-ndarray t-inv))

  (define inv-m (tensor-ndarray (tensor-inverse-3x3 t)))
  
  ;; 3x3 in upper left corner is M
  (ndarray-set! a-inv type 0 0 (ndarray-ref inv-m type 0 0))
  (ndarray-set! a-inv type 0 1 (ndarray-ref inv-m type 0 1))
  (ndarray-set! a-inv type 0 2 (ndarray-ref inv-m type 0 2))
  
  (ndarray-set! a-inv type 1 0 (ndarray-ref inv-m type 1 0))
  (ndarray-set! a-inv type 1 1 (ndarray-ref inv-m type 1 1))
  (ndarray-set! a-inv type 1 2 (ndarray-ref inv-m type 1 2))
  
  (ndarray-set! a-inv type 2 0 (ndarray-ref inv-m type 2 0))
  (ndarray-set! a-inv type 2 1 (ndarray-ref inv-m type 2 1))
  (ndarray-set! a-inv type 2 2 (ndarray-ref inv-m type 2 2))

  ;; 3x1 in upper right is -(inverse of m) * b
  (define b (vector (ndarray-ref a type 0 3) (ndarray-ref a type 1 3) (ndarray-ref a type 2 3)))
  (ndarray-set! a-inv type 0 3 (dot3 (vector (fl* -1.0 (ndarray-ref inv-m type 0 0)) (fl* -1.0 (ndarray-ref inv-m type 0 1)) (fl* -1.0 (ndarray-ref inv-m type 0 2))) b))
  (ndarray-set! a-inv type 1 3 (dot3 (vector (fl* -1.0 (ndarray-ref inv-m type 1 0)) (fl* -1.0 (ndarray-ref inv-m type 1 1)) (fl* -1.0 (ndarray-ref inv-m type 1 2))) b))
  (ndarray-set! a-inv type 2 3 (dot3 (vector (fl* -1.0 (ndarray-ref inv-m type 2 0)) (fl* -1.0 (ndarray-ref inv-m type 2 1)) (fl* -1.0 (ndarray-ref inv-m type 2 2))) b))

  ;; bottom row is 0 0 0 1
  (ndarray-set! a-inv type 3 3 1.0)

  t-inv)

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

;; todo: return the row permutations as well
(define (tensor-lu-decomp t)
  (define type (tensor-type t))
  (define shape (tshape t))
  (when (not (= (vector-length shape) 2))
    (error "tensor must be two dimensional matrix"))
  (when (tensor-iter t)
    (error "cannot take inverse of tensor slice"))
  (case (ctype->layout type)
    [(double)
     (define nda (ndarray_lu_decomp_double (tensor-ndarray t) (box 1.0)))
     (tensor type shape nda)]
    ;[(float) (tensor type shape (ndarray_mat_inverse_float (tensor-ndarray t)))]
    [else
     (error "unsupported tensor type" type)]))

(define (tensor-mat-inverse t)
  (define type (tensor-type t))
  (define shape (tshape t))
  (when (not (= (vector-length shape) 2))
    (error "tensor must be two dimensional matrix"))
  (when (tensor-iter t)
    (error "cannot take inverse of tensor slice"))
  (case (ctype->layout type)
    [(double) (tensor type shape (ndarray_mat_inverse_double (tensor-ndarray t)))]
    ;[(float) (tensor type shape (ndarray_mat_inverse_float (tensor-ndarray t)))]
    [else
     (error "unsupported tensor type" type)]))


