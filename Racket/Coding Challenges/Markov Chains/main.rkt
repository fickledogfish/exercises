#|
                                 Markov Chains
                                 =============
|#

#lang racket

(define text "the theremin is theirs, ok? yes, it is. this is a theremin.")

;; Extracts all the n-grams of the specified order from the text.
;;
;; contract:
;;     String NonNegativeInteger -> (ListOf String)
(define (extract-ngrams text order)
  (for/list ([idx (- (string-length text) order -1)])
    (substring text idx (+ idx order))))

;; Makes a list of pairs, where the first elements correspond to the unique
;; elements in the provided list, and the second is the total number of
;; occurencies said element had in the original list.
;;
;; contract:
;;     (ListOf String) -> (ListOf (ListOf String Integer))
(define (get-unique-element-count lst)
  (for/list ([unique-el (remove-duplicates lst)])
    (list unique-el
          (count (lambda (el) (string=? el unique-el)) lst)
          empty)))

;; Sorts the provided list according to each sublist's second elemnt.
;;
;; contract:
;;     (ListOf (ListOf String Integer)) -> (ListOf (ListOf String Integer))
(define (sort-by-most-common lst)
  (sort lst
        (lambda (el1 el2)
          (> (second el1) (second el2)))))

;;(println (extract-ngrams text 3))
(println (sort-by-most-common
          (get-unique-element-count (extract-ngrams text 3))))
