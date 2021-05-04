#lang racket

(require (only-in threading ~>))

;; Read the source file into a list of lower-cased words.
(define (extract-word-list filename)
  (~> (file->string filename)
      (string-replace _ #px"\\p{P}" " ")
      string-downcase
      string-split))

;; Searches the word database for a word that has the specified character at the
;; given position.
;;
;; contract:
;;     Character Integer (ListOf String) -> String
(define (search-words-for char index words)
  (let ([found? (member
                 char
                 words
                 (lambda (ch word)
                   (and ((string-length word) . > . index)
                        (equal? ch (string-ref word index)))))])
    (if found?
        (first found?)
        "")))

;; Runs the search function for each non-whitespace character in the seed
;;
;; contract
;;     String (ListOf String) -> (ListOf String)
(define (select-words seed words)
  (for/list ([ch seed]
             [index (in-naturals)])
    (if (char-whitespace? ch)
        ""
        (search-words-for ch index words))))

;; Create the poem by using the seed to extract words from the given file.
;;
;; contract:
;;     String String -> String
(define (create-poem seed filename)
  (~> (extract-word-list filename)
      (select-words (string-downcase seed) _)
      (filter (lambda (el) (not (string=? el ""))) _) ;; just for prettiness
      (string-join _ " ")))

;; Finally, make the poem and display it
(displayln (create-poem "SmItH" "test.txt"))
