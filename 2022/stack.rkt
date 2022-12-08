#lang racket

(require rackunit)

(define (make-stack) (list))

(define (push stack item) (append (list item) stack))

(define (head stack) (first stack))

(define (drop stack [n 1])
  (if
   (= n 1)
   (rest stack)
   (drop (rest stack) (- n 1))))

(check-equal?
 (let
     ([stack (make-stack)])
   (head (push stack 1)))
 1)

(check-equal?
 (let
     ([stack (make-stack)])
   (first (push (push stack 1) 2)))
 2)


(check-equal?
 (let
     ([stack (make-stack)])
   (first (drop (push (push stack 1) 2))))
 1)

