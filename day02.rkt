#lang racket

; processing input
(define (split-command command)
  (let ([ref-commend-list ((curry list-ref) (string-split command))])
    (cons (ref-commend-list 0) (string->number (ref-commend-list 1)))
    ))
(define commands (map split-command (file->lines "inputs/day02.txt")))

;first part
(define
  (cal-movement command)
  (define direction (car command))
  (define movement (cdr command))
  (match direction
    ["forward" (cons movement 0)]
    ["down" (cons 0 movement)]
    ["up" (cons 0 (* -1 movement))]))

(define
  (next-movement current-position command)
  (define movement (cal-movement command))
  (define current-x-position (car current-position))
  (define current-y-position (cdr current-position))
  (cons (+ current-x-position (car movement)) (+ current-y-position (cdr movement))))

(define final-position
  (foldl
   (lambda (current-command acc)
     (next-movement acc current-command))
   '(0 . 0)
   commands))

(* (car final-position) (cdr final-position))

;second part
