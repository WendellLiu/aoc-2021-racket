#lang racket

; processing input
(define (split-command command)
  (let ([ref-commend-list ((curry list-ref) (string-split command))])
    (cons (ref-commend-list 0) (string->number (ref-commend-list 1)))
    ))
(define commands (map split-command (file->lines "inputs/day02.txt")))

;first part
(define
  (cal-movement current-position command)
  (define direction (car command))
  (define movement (cdr command))
  (define current-x-position (car current-position))
  (define current-y-position (cdr current-position))
  (cons (+ current-x-position movement) (+ current-y-position movement)))

(foldl
 (lambda (current-command acc)
   (cal-movement acc current-command))
 '(0 . 0)
 commands)

;second part
