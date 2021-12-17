#lang racket

(define nums (map string->number (file->lines "inputs/day01.txt")))

(car (foldl (lambda (cV acc)
         (if (> cV (cdr acc)) 
           (cons (+ (car acc) 1) cV)
           (cons (car acc)  cV)
           )) 
       '(0 . +inf.0) nums)
)
