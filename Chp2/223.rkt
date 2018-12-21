#lang sicp

;; Chp 2.2.3

(define (square x)
  (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;; Nested map


;; Exercise
;; 2.33

(define (my-map proc sequence)
  (accumulate (lambda (x y)
                (cons (proc x) y)) nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
;    MISTAKEN   (+ (* x this-coeff) (car higher-terms)))
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
