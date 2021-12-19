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

(define final-position1
  (foldl
   (lambda (current-command acc)
     (next-movement acc current-command))
   '(0 . 0)
   commands))

(* (car final-position1) (cdr final-position1))

;second part
;'(x y aim)
(define
  (cal-movement-with-aim command aim)
  (define direction (car command))
  (define movement (cdr command))
  (match direction
    ["forward" (list movement (* aim movement) 0)]
    ["down" (list 0 0 movement)]
    ["up" (list 0 0 (* movement -1))]))

(define
  (next-movement-with-aim current-position command)
  (define current-x-position (list-ref current-position 0))
  (define current-y-position (list-ref current-position 1))
  (define current-aim (list-ref current-position 2))
  (define movement (cal-movement-with-aim command current-aim))
  (list
   (+ current-x-position (list-ref movement 0))
   (+ current-y-position (list-ref movement 1))
   (+ current-aim (list-ref movement 2))))

(define final-position2
  (foldl
   (lambda (current-command acc)
     (next-movement-with-aim acc current-command))
   '(0 0 0)
   commands))

(* (list-ref final-position2 0) (list-ref final-position2 1))

