#lang racket

(require (prefix-in lazy: lazy))

(module+ test
  (require (prefix-in ru: rackunit)))

#|
An infinitely lazy list of powers of two.
|#
(define powers-of-two
  (letrec ([aux (lambda (n)
                  (cons (delay (expt 2 n))
                        (delay (aux (add1 n)))))])
    (aux 0)))

(module+ test
  (ru:check-equal? 1 (force (lazy:first powers-of-two)))
  (ru:check-equal? 2 (force (lazy:second powers-of-two)))

  (ru:check-equal?
   '(1 2 4 8 16 32 64 128 256 512)
   (lazy:!! (lazy:take 10 powers-of-two))))

#|
A generic function on lazy infinite lists. Returns a list containing the first n
elements that satisfy the predicate.

Contract:
    (T -> Bool) (Lazy (ListOf T)) -> (ListOf T)
|#
(define (take-while prd lst)
  (define (aux prd lst acc)
    (let ([el (force (lazy:first lst))])
      (if (not (prd el))
          acc
          (aux prd (lazy:rest lst) (cons el acc)))))
  (aux prd lst empty))

(module+ test
  (ru:check-equal?
   '(8 4 2 1)
   (take-while (lambda (el) (< el 10)) powers-of-two)))

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

(module+ test
  (ru:check-equal? 64 (car (first (binary-repr 100))))

  (ru:check-equal?
   '((64 . #t) (32 . #t) (16 . #f) (8 . #f) (4 . #t) (2 . #f) (1 . #f))
   (binary-repr 100)))

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
