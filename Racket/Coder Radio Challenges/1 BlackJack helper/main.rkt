#|
https://www.reddit.com/r/CoderRadio/comments/4t7oyt/episode_214_coding_challenge/

Assuming a standard 52 card deck write me a BlackJack helper that advises the
user on the correct play (Hit, Stand, Double, or Split) as suggested by the
basic strategy.

Assume that the player is playing 1 on 1 with the dealer.

Here is the basic strategy chart for reference:

    http://www.blackjack-chart.com/

---

Possible outcomes:
    h   : hit
    s   : stand
    d   : double if allowed, otherwise hit
    ds  : double if allowed, otherwise stand
    sp  : split
    x/h : surrender if allowed, otherwise hit
    x/p : surrender if allowed, otherwise split
    x/s : surrender if allowed, otherwise stand
|#

#lang typed/racket

(module+ test
  (require typed/rackunit))

(module+ test
  ;; valid numbers
  (check-true (convert-card "2"))
  (check-true (convert-card "5"))
  (check-true (convert-card "A"))

  ;; invalid numbers
  (check-false (convert-card "132"))
  (check-false (convert-card "235"))
  (check-false (convert-card "542"))

  ;; complex numbers
  (check-false (convert-card "0.0+0.5i"))
  (check-false (convert-card "1.0+1.5i"))
  (check-false (convert-card "2.0-0.5i"))

  ;; ridiculously invalid
  (check-false (convert-card "afsse"))
  (check-false (convert-card "eklch")))
#|
Checks if a given card is valid.

Valid cards are:
    - numbered ->  2~10
    - ace      ->     A
    - king     ->     K
    - queen    ->     Q
    - jack     ->     J
|#
(define (validate-card [card : String]) : Boolean
  (let* ([c : (U Number False) (string->number card)]
         [card-number : Real (if (real? c) c 0)])
    (or
     ;; attempt to find lettered cards
     (string=? card "A")
     (string=? card "J")
     (string=? card "Q")
     (string=? card "K")

     ;; attempt to find valid numbered cards
     (and (card-number . >= . 2) (card-number . <= . 10)))))

(module+ test
  ;; numbers and royalty
  (check-eq? (convert-card "2") 2)
  (check-eq? (convert-card "4") 4)
  (check-eq? (convert-card "J") 10)
  (check-eq? (convert-card "Q") 10)
  (check-eq? (convert-card "K") 10)

  ;; aces get 1 when royalty is not present and 11 otherwise
  (check-eq? (convert-card "A" false) 1)
  (check-eq? (convert-card "A" true) 11))
#|
Converts a single card to its value.

Each numbered card corresponds to its number in points; kings, queens and jacks
get 10; aces get 11 when royalty is present and 1 otherwise.

This assumes the cards are valid.
|#
(define (convert-card [card : String]
                      [JQK-present? : Boolean false])
  : Real
  ;; This attempts to convert a card to its value with string->number, then
  ;; check if it got a real number back or garbage. In the first case, the
  ;; card is numbered, so this function should return it; in the second case,
  ;; the card may be royalty or ace (since we are assuming the hand is
  ;; validated) before getting here.
  (let* ([c : (U Number False) (string->number card)]
         [card-number : Real (if (real? c) c 0)])
    (cond
      ;; aces get 1 or 11
      [(string=? card "A")
       (if JQK-present? 11 1)]

      ;; kings, queens and jacks get 10
      [(or (string=? card "J")
           (string=? card "Q")
           (string=? card "K"))
       10]

      ;; valid number cards get their numbers
      [(and (card-number . >= . 2)
            (card-number . <= . 10))
       card-number]

      ;; and anything else is invalid, so return 0
      [else card-number])))

(module+ test
  ;; valid hands (cannot use check-true because andmap may return a Real)
  (check-not-false (validate-hand '("2" "3" "A")))
  (check-not-false (validate-hand '("2" "3" "A")))
  (check-not-false (validate-hand '("2" "3" "A")))

  ;; one invalid card
  (check-false (validate-hand '("2" "3" "S")))
  (check-false (validate-hand '("2" "3d" "A")))
  (check-false (validate-hand '("23" "3" "A")))

  ;; all invalid cards
  (check-false (validate-hand '("11" "d3d" "S")))
  (check-false (validate-hand '("21" "323" "S32")))
  (check-false (validate-hand '("qd" "3d3" "3S3"))))
#|
Validates a hand provided by the user.
|#
(define (validate-hand [hand : (Listof String)]) : Boolean
  ;; needs the if to get back a pure boolean
  (if (andmap validate-card hand)
      true
      false))

(module+ test
  (check-true (is-royalty-present? '("2" "3" "K")))
  (check-true (is-royalty-present? '("2" "Q" "3")))
  (check-false (is-royalty-present? '("2" "3" "4")))
  (check-false (is-royalty-present? '("2" "A" "4"))))
#|
Checks if the given hand has any Ks, Qs or Js.

This function does not check for a valid hand.
|#
(define (is-royalty-present? [hand : (Listof String)]) : Boolean
  ;; this needs to be like this to get a pure boolean out of the
  ;; function
  (if (or (member "K" hand)
          (member "Q" hand)
          (member "J" hand))
      true
      false))

(module+ test
  (check-eq? (sum-user-hand '("2" "3" "A")) 6))
#|
Sum the user's hand.

This function assumes hand is already verifyed.
|#
(define (sum-user-hand [hand : (Listof String)]) : Real
  (apply + (map convert-card hand)))
