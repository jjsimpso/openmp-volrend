#lang racket
 
;; Code for the book Chaos, Fractals, and Dynamics: Computer Experiments in Mathematics.
;; 

(require racket/gui/base
         racket/flonum
         "tensor.rkt")

(define (make-quadratic-func c)
  (lambda (x) (+ (* x x) c)))

;; define a canvas that displays a bitmap when its on-paint
;; method is called
(define bitmap-canvas%
  (class canvas%
    (init-field [bitmap #f])
    (inherit get-dc)
    (define/override (on-paint)
      (send (get-dc) draw-bitmap bitmap 0 0))
    (super-new)))

;; create a window that displays bitmap
(define (show-bitmap bitmap w h)
  (define frame (new frame%
                     [label "Bitmap"]
                     [width w]
                     [height h]))
  (new bitmap-canvas% [parent frame] [bitmap bitmap])
  (send frame show #t))

(define (linspace start stop [num 50] [endpoint? #t])
  (define num-points (if endpoint? (add1 num) num))
  (define shape (vector num-points))
  (define indexes (make-tensor shape 'index #:ctype _double))

  (define a-start (make-tensor (vector 1) (->fl start) #:ctype _double))
  (define a-span (make-tensor (vector 1)  (->fl (- stop start)) #:ctype _double))
  (define a-step (make-tensor (vector 1)  (exact->inexact (/ 1 num)) #:ctype _double))

  (t+ a-start (t* a-span (t* a-step indexes))))

;; z is a complex number
(define (orbit-escapes f escape-value before-iter initial-z)
  (let loop ([z initial-z]
             [i 0])
    (cond [(fl> (magnitude z) escape-value) #t]
          [(>= i before-iter) #f]
          [else (loop (f z) (+ i 1))])))

;; ex: (plot-julia-set (make-quadratic-func (make-rectangular -1 0)) 20 2.0 4 4 200 200)
(define (plot-julia-set f escape-iter escape-magnitude x-axis-length y-axis-length width height)
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  (define fill-color (bytes 255 0 0 0));(make-color 0 0 0))
  (define back-color (bytes 255 255 255 255));(make-color 255 255 255))
  
  (define x-scale (/ x-axis-length width))
  (define y-scale (/ y-axis-length height))
  (define argb-pixels (make-bytes (* width height 4) 0))
  (define row-bytes (* width 4))
  
  (printf "Calculating~n")
  
  (for* ([x (in-range width)]
         [y (in-range height)])

    ;(printf ".")

    ; using make-flrectangular makes a huge performance difference with some julia sets
    (if (orbit-escapes f escape-magnitude escape-iter 
                       (make-flrectangular (+ (- (/ x-axis-length 2.0))
                                              (* x x-scale))
                                           (- (/ y-axis-length 2.0)
                                              (* y y-scale))))
        (bytes-copy! argb-pixels (+ (* y row-bytes) (* x 4)) back-color 0 4)
        (bytes-copy! argb-pixels (+ (* y row-bytes) (* x 4)) fill-color 0 4)))

  (send dc set-argb-pixels 0 0 width height argb-pixels)
  (send dc get-bitmap))

;; ex: (plot-mandelbrot-set 30 2.0 4 4 300 300)
(define (plot-mandelbrot-set escape-iter escape-magnitude x-axis-length y-axis-length width height)
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  (define fill-color (bytes 255 0 0 0));(make-color 0 0 0))
  (define back-color (bytes 255 255 255 255));(make-color 255 255 255))
  
  (define x-length (exact-truncate (/ x-axis-length 2.0)))
  (define y-length (exact-truncate (/ y-axis-length 2.0)))
  (define argb-pixels (make-bytes (* width height 4) 0))
  (define row-bytes (* width 4))

  (printf "Calculating~n")
  (define xsteps (linspace (- x-length) x-length (sub1 width)))
  (define ysteps (linspace (- y-length) y-length (sub1 height)))
  (define hminus1 (sub1 height))
  (define c (make-tensor (vector height width) 0.0 #:ctype _complex))  
  (for* ([y (in-range height)]
         [x (in-range width)])
    (tset! c y x (make-flrectangular (tref xsteps x) (tref ysteps (- hminus1 y)))))

  #;(for* ([y (in-range height)]
         [x (in-range width)])
    (printf "(~a,~a) = ~a~n" x y (tref c y x)))
  
  (define m (make-tensor (vector height width) (make-flrectangular 0.0 0.0) #:ctype _complex))
  (for ([i (in-range escape-iter)])
    (set! m (t+ (t* m m) c)))

  (for* ([y (in-range height)]
         [x (in-range width)])
    ;(printf "(~a,~a) mag = ~a~n" x y (tref m y x))
    (when (fl<= (magnitude (tref m y x)) escape-magnitude)
      (bytes-copy! argb-pixels (+ (* y row-bytes) (* x 4)) fill-color 0 4)))
  
  (send dc set-argb-pixels 0 0 width height argb-pixels)
  (send dc get-bitmap))

(define mandelbrot-frame% 
  (class frame%

    (init-field [x-axis-length 4])
    (init-field [y-axis-length 4])

    (super-new)
    
    (inherit get-width)
    (inherit get-height)

    (define/override (on-subwindow-event receiver event)
      (define type-of-event (send event get-event-type))

      (cond [(eq? type-of-event 'left-down)
             (define x (send event get-x))
             (define y (send event get-y))
             (define x-scale (/ x-axis-length (get-width)))
             (define y-scale (/ y-axis-length (get-height)))

             (define f (make-quadratic-func (make-flrectangular (+ (- (/ x-axis-length 2.0))
                                                                   (* x x-scale))
                                                                (- (/ y-axis-length 2.0)
                                                                   (* y y-scale)))))
             (show-bitmap
              (plot-julia-set f 20 2.0 4 4 (get-width) (get-height)) 
              ;(orbit-diagram make-quadratic-func 0 200 50 (range -2 0.25) (get-width) (get-height))
              (get-width)
              (get-height))]
            [else #f]))
    
    ))

    


(define (show-mandelbrot w h)
  (define frame (new mandelbrot-frame%
                     [label "Mandelbrot Set"]
                     [width w]
                     [height h]))

  (define bitmap (plot-mandelbrot-set 30 2.0 4 4 w h))

  (new bitmap-canvas% [parent frame] [bitmap bitmap])
  (send frame show #t))
