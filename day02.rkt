#lang racket

; processing input
(define (split-command command) (let ([ref-commend-list ((curry list-ref) (string-split command))])
                                  (cons (ref-commend-list 0) (ref-commend-list 1))
                                  ))
(define commands (map split-command (file->lines "inputs/day02.txt")))

;first part
;(car (foldl
;(lambda (cV acc)
;(define count (car acc))
;(define lastValue (cdr acc))
;(if (> cV lastValue)
;(cons (+ count 1) cV)
;(cons count  cV)
;))
;'(0 . +inf.0) commands))

;second part
