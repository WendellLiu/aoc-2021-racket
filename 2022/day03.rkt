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

(define (string-to-hash str-list)
  (foldl (lambda (value acc)
           (hash-set acc value #t))
         #hash()
         str-list))

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

(define (group-by-num l n)
  (foldl (lambda (value acc)
           (if
            (= n (length (cdr acc)))
            (cons acc (list value))
            (cons (car acc) (append (cdr acc) (list value)))))
         (cons null '())
         l))

(check-equal? (group-by-num '(1 2 3 4 5 6) 3) (cons (cons null '(1 2 3)) '(4 5 6)))

(define (get-badge-type l)
  (define l2 (map string->list l))
  (let ([table1 (string-to-hash (first l2))] [table2 (string-to-hash (second l2))] [third-str (third l2)])
    (findf (lambda (char)
             (and (hash-has-key? table1 char) (hash-has-key? table2 char))) third-str)))

(define get-score-from-group (compose1 map-char-to-score get-badge-type))

(define (get-score-part2 pair)
  (if
   (empty? (car pair))
   (get-score-from-group (cdr pair))
   (+ (get-score-from-group (cdr pair)) (get-score-part2 (car pair)))))

(check-equal? (get-score-part2 (cons (cons null '("asPf" "zegP" "Pwbv")) '("Jasd" "zxJc" "vJbn"))) 78)

(define (extract-result-list-part2 path)
  (get-score-part2 (group-by-num (file->lines path) 3)))

(check-equal? (extract-result-list-part2 path-of-test-source) 70)

(displayln (extract-result-list-part2 path-of-source))

