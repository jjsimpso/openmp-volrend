#lang racket

(require racket/flonum
         "tensor.rkt")

(require rackunit)

(define (linspace start stop [num 50] [endpoint? #t])
  (define num-points (if endpoint? (add1 num) num))
  (define shape (vector num-points))
  (define indexes (make-tensor shape 'index #:ctype _double))

  (define a-start (make-tensor (vector 1) (->fl start) #:ctype _double))
  (define a-span (make-tensor (vector 1)  (->fl (- stop start)) #:ctype _double))
  (define a-step (make-tensor (vector 1)  (exact->inexact (/ 1 num)) #:ctype _double))

  (t+ a-start (t* a-span (t* a-step indexes))))

(define (test-linspace)
  (define result (linspace 0 5000 50 #t))
  (check-equal?
   (map (lambda (i) (tref result i)) '(0 1 2 3 4 5 50))
   '(0.0 100.0 200.0 300.0 400.0 500.0 5000.0)))
