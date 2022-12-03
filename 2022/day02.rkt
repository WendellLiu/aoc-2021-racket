#lang racket

(define path-of-source "inputs/test-day02.txt")
;(define path-of-source "inputs/day02.txt")

(define (map-opp-to-symbol c)
  (match c
    ["A" 'R]
    ["B" 'P]
    ["C" 'S]))

(define (map-mine-to-symbol c)
  (match c
    ["X" 'R]
    ["Y" 'P]
    ["Z" 'S]))

(define (split-command command)
  (let ([l (string-split command)])
    (list (map-opp-to-symbol (list-ref l 0)) (map-mine-to-symbol (list-ref l 1)))))


(define shape-score-table (hash 'R 1 'P 2 'S 3))

(define (get-shape-score l pos)
  (hash-ref shape-score-table (list-ref l pos)))

(define rock-table (hash 'P 'L 'R 'D 'S 'W))
(define scissors-table (hash 'P 'W 'R 'L 'S 'D))
(define paper-table (hash 'P 'D 'R 'W 'S 'L))
(define outcome-table (hash 'R rock-table 'S scissors-table 'P paper-table))

(define (get-outcome-score l)
  (let ([opp (list-ref l 0)] [self (list-ref l 1)])
    (match (hash-ref (hash-ref outcome-table self) opp)
      ['L 0]
      ['D 3]
      ['W 6])))

(displayln (get-outcome-score (split-command "A Z")))

(define (extract-result-list path)
  (with-input-from-file path
    (lambda ()
      (1)
      )))


