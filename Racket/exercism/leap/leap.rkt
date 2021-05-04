#lang racket

(require
 (only-in math/number-theory
          divides?))

(provide leap-year?)

(define (leap-year? year)
  (and (divides? 4 year)
       (or (not (divides? 100 year))
           (divides? 400 year))))
