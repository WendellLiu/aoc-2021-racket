#lang racket

;(define path-of-source "inputs/test-day01.txt")
(define path-of-source "inputs/day01.txt")

(define (extract-result-list path)
  (with-input-from-file path
    (lambda ()
      (for/fold ([acc '(0)]) ([i (in-lines)])
        (define num (if (non-empty-string? i) (string->number i) 0))
        (if (= 0 num) (cons 0 acc) (list-set acc 0 (+ (car acc) num)))))))

(define result-list (extract-result-list path-of-source))
(displayln result-list)

(define (list->max l) (foldl
                       (lambda (value acc) (if (> acc value) acc value)) 0 l))

(displayln (list->max result-list))

(define (top n) (take (sort result-list >) n))

(displayln (for/sum ([i (top 3)]) i))







