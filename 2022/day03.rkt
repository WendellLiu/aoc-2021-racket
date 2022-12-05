#lang racket

(require rackunit)

(define path-of-test-source "inputs/test-day03.txt")
(define path-of-source "inputs/day03.txt")

(define alphabet-list
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define score-table
  (for/fold
   ([table #hash()])
   ([c (string->list alphabet-list)]
    [index (in-range (string-length alphabet-list)) ])
    (hash-set table c (+ index 1))))

(define (map-char-to-score char) (hash-ref score-table char))

(define (split-to-pair str)
  (let
      ([pos (/ (string-length str) 2)] [l (string->list str)])
    (cons (take l pos) (drop l pos))))

(check-equal? (split-to-pair "asPfzegP") (cons '(#\a #\s #\P #\f) '(#\z #\e #\g #\P)))

(define (string-to-hash str)
  (foldl (lambda (value acc)
           (hash-set acc value #t))
         #hash()
         str))

(define (match-char char-hash str)
  (findf (lambda (char)
           (hash-has-key? char-hash char)) str))

(define (get-match-char pair)
  (let ([table (string-to-hash (car pair))])
    (match-char table (cdr pair))))

(define get-score (compose1 map-char-to-score get-match-char split-to-pair))

(check-equal? (get-score "asPfzegP") 42)

(define (extract-result-list path)
  (with-input-from-file path
    (lambda ()
      (for/fold ([acc 0]) ([i (in-lines)])
        (+ acc (get-score i))))))


(check-equal? (extract-result-list path-of-test-source) 157)

(displayln (extract-result-list path-of-source))

