#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

(require "ndarray-ffi.rkt")

(provide read_ppm
         write_ppm
         ndarray_read_ppm
         ndarray_write_ppm
         ndarray_iter_write_ppm)

(define (check-null p who)
  (if (false? p)
      (error who "returned NULL")
      p))

;(define free_ppm (deallocator (lambda (v) (when v (free v)))))

;; unable to get the deallocator to work with read_ppm returning three arguments
;; for now we will require manually calling free
(define-ndarray read_ppm (_fun _path [w : (_ptr o _int)] [h : (_ptr o _int)]
                               -> (r : _uint8-pointer)
                               -> (values r w h)))
;;  #:wrap (allocator free_ppm))

(define-ndarray write_ppm (_fun _path _int _int _pointer -> _int))

(define-ndarray ndarray_read_ppm (_fun _path
                                       -> (p : _NDArray-pointer/null)
                                       -> (check-null p 'ndarray_read_ppm))
  #:wrap (allocator ndarray_free))

(define-ndarray ndarray_write_ppm (_fun _NDArray-pointer _path -> _int))

(define-ndarray ndarray_iter_write_ppm (_fun _NDArrayIter-pointer _path _int _int -> _int))
