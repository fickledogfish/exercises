#lang racket

(require (only-in lazy take !! first rest))

#|
An infinitely lazy list of powers of two.
|#
(define powers-of-two
  (letrec ([aux (lambda (n)
                  (cons (delay (expt 2 n))
                        (delay (aux (add1 n)))))])
    (aux 0)))

#;(displayln (!! (take 10 powers-of-two)))

#;(displayln (force (first powers-of-two)))
#;(displayln (rest powers-of-two))

#|
A generic function on lazy infinite lists. Returns a list containing the first n
elements that satisfy the predicate.

Contract:
    (T -> Bool) (Lazy (ListOf T)) -> (ListOf T)
|#
(define (take-while prd lst)
  (define (aux prd lst acc)
    (let ([el (force (first lst))])
      (if (not (prd el))
          acc
          (aux prd (rest lst) (cons el acc)))))
  (aux prd lst empty))

#;(displayln (take-while (lambda (el) (< el 10)) powers-of-two))

#|
Transform a number into a list of pairs, representing the binary form of that
number.

Contract:
    Integer -> (ListOf (Pair Integer Bool))
|#
(define (binary-repr n)
  (define (aux n powers acc)
    (let* ([power           (first powers)]
           [leftover-powers (rest powers)]
           [rem             (remainder n power)]
           [qt              (quotient n power)]
           [divisible       (> qt 0)]
           [accumulator     (cons (cons power divisible) acc)])
      (if (empty? leftover-powers)
          (reverse accumulator)
          (aux rem leftover-powers accumulator))))

  (aux n (take-while (lambda (el) (<= el n)) powers-of-two) empty))

#;(displayln (binary-repr 100))

#;(displayln (car (first (binary-repr 100))))

#|
Transform the binary number into a string.

Contract:
    (ListOf (Pair Integer Bool)) -> String
|#
(define (to-binary-string n)
  (list->string
   (map (lambda (i)
          (if (cdr i)
              #\1
              #\0))
        (binary-repr n))))

(define target 342)

(displayln (to-binary-string target))
