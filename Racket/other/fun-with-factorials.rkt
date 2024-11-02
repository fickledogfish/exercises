#lang typed/racket

(module+ test
  (require (prefix-in ru: typed/rackunit)))

(: recursive-factorial (-> Integer Integer))
(define (recursive-factorial n)
  (if (> n 1)
      (* n (recursive-factorial (sub1 n)))
      1))

(module+ test
  (ru:check-= 1 (recursive-factorial 1) 0)
  (ru:check-= 2 (recursive-factorial 2) 0)
  (ru:check-= 6 (recursive-factorial 3) 0))

(: tree-style-factorial (-> Integer Integer))
(define (tree-style-factorial n)
  (: multiply-range (-> Integer Integer Integer))
  (define (multiply-range start end)
    (if (= start end)
        start
        (let* ([midpoint-left (floor (/ (+ start end) 2))]
               [midpoint-right (min (add1 midpoint-left) end)])
          (* (multiply-range start midpoint-left)
             (multiply-range midpoint-right end)))))

  (multiply-range 1 n))

(module+ test
  (ru:check-= 1 (tree-style-factorial 1) 0)
  (ru:check-= 2 (tree-style-factorial 2) 0)
  (ru:check-= 6 (tree-style-factorial 3) 0)
  (ru:check-= 362880 (tree-style-factorial 9) 0)
  (ru:check-= 3628800 (tree-style-factorial 10) 0)
  (ru:check-= 2432902008176640000 (tree-style-factorial 20) 0)
  (ru:check-= 265252859812191058636308480000000 (tree-style-factorial 30) 0)
  (ru:check-= 30414093201713378043612608166064768844377641568960512000000000000 (tree-style-factorial 50) 0))

(let ([n 10000]
      [tests (list recursive-factorial
                   tree-style-factorial)])
  (for ([test tests])
    (display (object-name test))
    (display " => ")
    (time (test n))))
