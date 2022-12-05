#lang racket

;(define path-of-source "inputs/test-day02.txt")
(define path-of-source "inputs/day02.txt")

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

(define (get-shape-score l)
  (hash-ref shape-score-table (list-ref l 1)))

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

(define (sum-score l) (+ (get-shape-score l) (get-outcome-score l)))

(define (extract-result-list path)
  (with-input-from-file path
    (lambda ()
      (for/fold ([acc 0]) ([i (in-lines)])
        (+ acc (sum-score (split-command i)))))))

(displayln (extract-result-list path-of-source))

(define rock-match-table (hash 'L 'S 'D 'R 'W 'P))
(define scissors-match-table (hash 'L 'P 'D 'S 'W 'R))
(define paper-match-table (hash 'L 'R 'D 'P 'W 'S))
(define outcome-match-table (hash 'R rock-match-table 'S scissors-match-table 'P paper-match-table))
(define (get-outcome-code-to-shape-score l)
  (let ([opp (list-ref l 0)] [outcome-code (list-ref l 1)])
    (hash-ref shape-score-table (hash-ref (hash-ref outcome-match-table opp) outcome-code)
              )))

(define (map-outcome-code-to-symbol c)
  (match c
    ["X" 'L]
    ["Y" 'D]
    ["Z" 'W]))

(define (split-command-2 command)
  (let ([l (string-split command)])
    (list (map-opp-to-symbol (list-ref l 0)) (map-outcome-code-to-symbol (list-ref l 1)))))

(define (get-outcome-score-part2 l)
  (let ([outcome (list-ref l 1)])
    (match outcome
      ['L 0]
      ['D 3]
      ['W 6])))

(define (sum-score-part2 l) (+ (get-outcome-code-to-shape-score l) (get-outcome-score-part2 l)))
(define (extract-result-list-part2 path)
  (with-input-from-file path
    (lambda ()
      (for/fold ([acc 0]) ([i (in-lines)])
        (+ acc (sum-score-part2 (split-command-2 i)))))))

(displayln (extract-result-list-part2 path-of-source))


