#lang racket

(define nums (map string->number (file->lines "inputs/day01.txt")))

;first part
(car (foldl
      (lambda (cV acc)
        (define count (car acc))
        (define last-value (cdr acc))
        (if (> cV last-value)
            (cons (+ count 1) cV)
            (cons count  cV)
            ))
      '(0 . +inf.0) nums))

;second part
(define (sum-list l) (for/sum ([i l]) i))

(car (foldl
      (lambda (cV acc)
        (define count (car acc))
        (define last-window (cdr acc))
        (cond
          [(< (length last-window) 3) (cons count (append last-window (list cV)))]
          [else (let
                    (
                     [current-window (append (rest last-window) (list cV))]
                     [lastSum (sum-list last-window)]
                     )
                  (cons (if
                         (> (sum-list current-window) lastSum)
                         (+ count 1)
                         count
                         ) current-window))]
          ))
      (cons 0 '()) nums))

