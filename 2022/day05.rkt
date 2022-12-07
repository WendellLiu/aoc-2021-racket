#lang racket

(require rackunit)

(define path-of-test-source "inputs/test-day05.txt")
(define path-of-source "inputs/day05.txt")

(define (split-to-two l)
  (foldl (lambda (line acc)
           (let ([sanitized-line (string-trim line)])
             (if
              (non-empty-string? sanitized-line)
              (append acc (list line))
              (append (list acc) (list))
              )))
         (list)
         l
         ))

(define get-stack-lines car)
(define get-command-lines cdr)

(define (get-vector-size lines)
  (let
      ([numbers ((compose1 ((curry map) string->number) string-split last) lines)])
    (apply max numbers)))

(check-equal? (get-vector-size (list " " " " "1 2 3")) 3)

(define (extract-result-list path)
  (split-to-two (file->lines path)))

(displayln (extract-result-list path-of-test-source))


; 1 + x * 4 = y
; (y - 1) / 4 = x
; x: pos of vector
; y pos of char in string

