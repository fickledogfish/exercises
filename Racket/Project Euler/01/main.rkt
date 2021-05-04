#|
                        Problem 1: Multiples of 3 and 5
                        ===============================


If we list all the natural numbers below 10 that are multiples of 3 or 5, we get
3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
|#

#lang typed/racket

(module+ test
  (require typed/rackunit))

;; Provided example: the sum of the multiples of 3 or 5 below 10 is 23.
(module+ test
  (check-= (foldl + 0 (get-multiples 10)) 23
           0
           "Could not verify given example fo proposed solution."))

#|
Returns a list where all non-zero elements are multiples of 3 or 5.

It does not matter that a zero is added instead of adding no value at all, since
we'll be folding this list.
|#
(define (get-multiples [cap : Nonnegative-Integer])
  : (Listof Integer)
  (for/list ([num (in-range cap)])
    (if (or ((num . modulo . 3) . = . 0)
            ((num . modulo . 5) . = . 0))
        num
        0)))

;; Do the work
;; (foldl + 0 (get-multiples 1000))

#|
                              Alternative Solution
                              ====================

(Found in the paper accessed by finding the right answer.)

Suppose we have a function that sums all numbers less than cap that are
multiples of n:

    (define (sum-divisable-by n cap)
      ...)

Then, we could calculate (sum-divisable-by 3 1000) and (sum-divisable-by 3
1000). However, some numbers that are divisible by 3 and 5 (meaning numbers that
are divisible by 15) will be counted twice. We can solve that by subtracting
them from the final value. Then, this problem can be calculated as:

    (- (+ (sum-divisable-by 3 1000) (sum-divisable-by 5 1000))
       (sum-divisable-by 15 1000))

The function `sum-divisable-by` can be calculated in a smart way, so that the
program becomes more efficient. So, note that

    sum-divisable-by(3 1000) = 3 + 6 + 9 + ... + 999

and

    sum-divisable-by(5 1000) = 5 + 10 + 15 + ... + 995

If we consider integer division (//, rounds the division to the nearest
integer), we can see that

    199 = 995//5 = 999//3

Now, note that

    1 + 2 + 3 + ... + p = p*(p + 1)/2

This means we can calculate each sum of divisables simply:

    p = cap // n
    sum-divisable-by(n cap) = n*p*(p + 1)//2

Most important of all, note that cap is defined as inclusive here, but in the
racket code it should be exclusive.
|#
(module+ test
  (check-= (- (+ (sum-divisable-by 3 10)
                 (sum-divisable-by 5 10))
              (sum-divisable-by 15 10))
           23
           0
           "Could not verify given example for the alternative solution."))

(define (sum-divisable-by [n : Nonnegative-Integer]
                          [cap : Nonnegative-Integer])
  : Integer

  (define p ((sub1 cap) . quotient . n))

  ((* n p (add1 p)) . quotient . 2))

;; Do the work
(displayln (- (+ (sum-divisable-by 3 1000)
                 (sum-divisable-by 5 1000))
              (sum-divisable-by 15 1000)))
