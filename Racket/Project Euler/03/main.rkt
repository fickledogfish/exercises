#|
WRONG

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
|#

#lang racket

(define (get-largest-factor-of n
                               [factor 3]
                               [largest-so-far 0])
  (if (factor . < . (sub1 n))

      (let ([divisible? (= (n . modulo . factor) 0)])
        (get-largest-factor-of (if divisible?
                                   (/ n factor)
                                   n)
                               (+ 2 factor)
                               (if divisible?
                                   factor
                                   largest-so-far)))

      largest-so-far))

(get-largest-factor-of 600851475143)
