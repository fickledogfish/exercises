#|
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
the 6th prime is 13.

What is the 10 001st prime number?
|#

#lang racket

(define (is-divisible-by-any-in n lst)
  (andmap (lambda (i) ((n . remainder . i) . = . 0)) lst))

(define (build-prime-list n [lst empty])
  (if (= (length lst) n)
      lst
      (build-prime-list n lst)))

(displayln (build-prime-list 3))
