#lang racket

(define nums (map string->number (file->lines "inputs/day01.txt")))

;first part
(car (foldl
       (lambda (cV acc)
         (define count (car acc))
         (define lastValue (cdr acc))
         (if (> cV lastValue) 
           (cons (+ count 1) cV)
           (cons count  cV)
           )) 
       '(0 . +inf.0) nums))

;second part
(define (sumList l) (for/sum ([i l]) i))

(car (foldl
       (lambda (cV acc)
         (define count (car acc))
         (define lastWindow (cdr acc))
         (cond 
           [(< (length lastWindow) 3) (cons count (append lastWindow (list cV)))]
           [else (let 
                   (
                    [currentWindow (append (rest lastWindow) (list cV))]
                   [lastSum (sumList lastWindow)]
                   ) 
                   (cons (if 
                      (> (sumList currentWindow) lastSum)
                      (+ count 1)
                      count
                           ) currentWindow))
                 ]
           ))
       (cons 0 '()) nums))
