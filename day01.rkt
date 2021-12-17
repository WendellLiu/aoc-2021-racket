#lang racket

(define nums (map string->number (file->lines "inputs/day01.txt")))

(car (foldl
       (lambda (cV acc)
         (define lastValue (cdr acc))
         (define count (car acc))
         (if (> cV lastValue) 
           (cons (+ count 1) cV)
           (cons count  cV)
           )) 
       '(0 . +inf.0) nums))
