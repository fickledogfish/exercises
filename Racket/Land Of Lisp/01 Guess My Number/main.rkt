#lang racket

(define (take-a-guess small large)
  (let ([new-val (round (/ (+ small large) 2))])
    (printf "~a ~n> " new-val)
    (values (read) new-val)))

(define (game small large)
  (define-values (result new-val) (take-a-guess small large))

  (cond
    [(or (= small new-val) (= new-val large))
     (displayln new-val)]

    [(or (eq? result 'bigger) (eq? result 'b))
     (game new-val large)]

    [(or (eq? result 'smaller) (eq? result 's))
     (game small new-val)]

    [else
     (displayln "Invalid input, please try again")
     (game small large)]))

(define (new-game)
  (define argv (vector->list (current-command-line-arguments)))
  (define argc (length argv))

  (when (not (= argc 2))
     (printf "Expected 2 arguments, got ~a~n" argc)
     (exit))

  (let* ([argv (map string->number argv)]
         [small (apply min argv)]
         [large (apply max argv)])
    (game small large)))

(new-game)
