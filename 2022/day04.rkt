#lang racket

(require rackunit)

(define path-of-test-source "inputs/test-day04.txt")
(define path-of-source "inputs/day04.txt")

(define (to-pairs l) (map (lambda (str)
                            (let ([split-list (string-split str ",")])
                              (cons (list-ref split-list 0) (list-ref split-list 1)))) l))

(check-equal? (to-pairs (list "2-4,6-8" "2-3,4-5")) (list (cons "2-4" "6-8") (cons "2-3" "4-5")))

(define (create-path-table start end) (hash 'start start 'end end))
(define (get-start table) (hash-ref table 'start))
(define (get-end table) (hash-ref table 'end))

(define (pos-string-to-table str)
  (let
      ([l (string-split str "-")])
    (create-path-table (string->number (first l)) (string->number (second l)))))

(define (pair-to-pos-table pair)
  (cons
   (pos-string-to-table (car pair))
   (pos-string-to-table (cdr pair))))

(check-equal? (pair-to-pos-table (cons "2-4" "6-8")) (cons (create-path-table 2 4) (create-path-table 6 8)))

(define (map-pair-to-pos-table pairs) (map pair-to-pos-table pairs))

(define (is-within path1 path2)
  (let
      ([start-diff (- (get-start path1) (get-start path2))]
       [end-diff (- (get-end path1) (get-end path2))]
       )
    (<= (* start-diff end-diff) 0)))

(check-equal? (is-within (create-path-table 1 4) (create-path-table 2 3)) #t)
(check-equal? (is-within (create-path-table 2 3) (create-path-table 1 4)) #t)
(check-equal? (is-within (create-path-table 1 3) (create-path-table 1 4)) #t)
(check-equal? (is-within (create-path-table 2 3) (create-path-table 5 6)) #f)

(define (map-is-within tables)
  (map (lambda
           (pair)
         (is-within (car pair) (cdr pair)))
       tables))

(define (sum-of-true l) (for/sum ([i (in-list l)]) (if i 1 0)))

(define get-number-within-path (compose1 sum-of-true map-is-within map-pair-to-pos-table to-pairs))

(define (extract-result-list path)
  (get-number-within-path (file->lines path)))

(check-equal? (extract-result-list path-of-test-source) 2)
(displayln (extract-result-list path-of-source))

(define (is-overlap path1 path2)
  (not (or
        (and (> (get-start path1) (get-end path2)) (> (get-end path1) (get-end path2)))
        (and (< (get-start path1) (get-start path2)) (< (get-end path1) (get-start path2))))))

(check-equal? (is-overlap (create-path-table 1 4) (create-path-table 2 3)) #t)
(check-equal? (is-overlap (create-path-table 2 3) (create-path-table 1 4)) #t)
(check-equal? (is-overlap (create-path-table 1 3) (create-path-table 1 4)) #t)
(check-equal? (is-overlap (create-path-table 2 3) (create-path-table 5 6)) #f)
(check-equal? (is-overlap (create-path-table 2 5) (create-path-table 4 6)) #t)
(check-equal? (is-overlap (create-path-table 4 6) (create-path-table 2 5)) #t)

(define (map-is-overlap tables)
  (map (lambda
           (pair)
         (is-overlap (car pair) (cdr pair)))
       tables))

(define get-number-overlap-path (compose1 sum-of-true map-is-overlap map-pair-to-pos-table to-pairs))

(define (extract-result-list-part2 path)
  (get-number-overlap-path (file->lines path)))

(check-equal? (extract-result-list-part2 path-of-test-source) 4)
(displayln (extract-result-list-part2 path-of-source))


