#lang racket

(provide sum-of-squares square-of-sums difference)

(define (sum-of-squares n)
  (for/sum ([i (in-range 1 (add1 n))]) (sqr i)))

(define (square-of-sums n)
  (sqr
   (/ (* n (add1 n)) 2)
   ;; (for/sum ([i (in-range 1 (add1 n))]) i)
   ))

(define (difference n)
  (- (square-of-sums n)
     (sum-of-squares n)))
