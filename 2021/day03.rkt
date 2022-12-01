#lang racket

(define path-of-source "inputs/day03.txt")

(define (length-of-vector path) (with-input-from-file path
                                  (lambda ()
                                    (string-length (read-line)))))

(define size-of-vector (length-of-vector path-of-source))


(define (length-of-file path) (with-input-from-file path
                                (lambda ()
                                  (for/sum ([i (in-lines)]) 1))))

(define size-of-source (length-of-file path-of-source))

(displayln size-of-source)

(define result-vec (make-vector size-of-vector 0))


(define char->int (compose string->number string))

(define
  (update-result-vec str vec)
  (for ([c str] [index size-of-vector])
    (vector-set! vec index (+ (char->int c) (vector-ref vec index)))))

(define (extract-result-vec path vec)
  (with-input-from-file path
    (lambda ()
      (for ([i (in-lines)]) (update-result-vec i vec))
      )))

(extract-result-vec path-of-source  result-vec)

(displayln result-vec)

(displayln (quotient size-of-source 2))

(define (to-gamma num) (if (> num (quotient size-of-source 2))  1  0))

(define gamma-vector
  (vector-map to-gamma result-vec))

(define epsilon-vector (vector-map (lambda (i) (if (= 1 i) 0 1)) gamma-vector))

(displayln gamma-vector)
(displayln epsilon-vector)

(define (binary-vector->integer vec)
  (for/sum ([int vec] [index size-of-vector])
    (* int (expt 2 (- (- size-of-vector 1) index)))))

(displayln (* (binary-vector->integer gamma-vector) (binary-vector->integer epsilon-vector)))




















