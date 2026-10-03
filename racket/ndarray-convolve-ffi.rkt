#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/cvector
         "ndarray-ffi.rkt")

(provide ndarray_convolve2d_point_uint8_t
         ndarray_convolve2d_point_uint16_t
         ndarray_convolve2d_point_vec3_uint8_t
         ndarray_convolve2d_point_vec3_uint16_t
         ndarray_convolve2d_uint8_t
         ndarray_convolve2d_uint16_t
         ndarray_convolve2d_vec3_uint8_t
         ndarray_convolve2d_vec3_uint16_t)
         
(define libvolrend (ffi-lib "../libvolrend"))

(define _convolve_mode
  (_enum '(NDARRAY_CONVOLVE_SKIP
           NDARRAY_CONVOLVE_CLAMP)))

(define-ndarray ndarray_convolve2d_point_uint8_t (_fun _NDArray-pointer _cvector _int _int _intptr _intptr -> _uint8))
(define-ndarray ndarray_convolve2d_point_uint16_t (_fun _NDArray-pointer _cvector _int _int _intptr _intptr -> _uint16))

(define-ndarray ndarray_convolve2d_point_vec3_uint8_t (_fun _NDArray-pointer _cvector _int _int _intptr _intptr _pointer -> _void))
(define-ndarray ndarray_convolve2d_point_vec3_uint16_t (_fun _NDArray-pointer _cvector _int _int _intptr _intptr _pointer -> _void))


(define-ndarray ndarray_convolve2d_uint8_t (_fun _NDArray-pointer _cvector _int _int _convolve_mode
                                                 -> (p : _NDArray-pointer/null)
                                                 -> (check-null p 'ndarray_convolve2d_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_convolve2d_uint16_t (_fun _NDArray-pointer _cvector _int _int _convolve_mode
                                                  -> (p : _NDArray-pointer/null)
                                                 -> (check-null p 'ndarray_convolve2d_uint16_t))
  #:wrap (allocator ndarray_free))


(define-ndarray ndarray_convolve2d_vec3_uint8_t (_fun _NDArray-pointer _cvector _int _int _convolve_mode
                                                      -> (p : _NDArray-pointer/null)
                                                      -> (check-null p 'ndarray_convolve2d_vec3_uint8_t))
  #:wrap (allocator ndarray_free))
(define-ndarray ndarray_convolve2d_vec3_uint16_t (_fun _NDArray-pointer _cvector _int _int _convolve_mode
                                                       -> (p : _NDArray-pointer/null)
                                                       -> (check-null p 'ndarray_convolve2d_vec3_uint16_t))
  #:wrap (allocator ndarray_free))
