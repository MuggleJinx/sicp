#lang sicp

;; Test
(define l '((x1 x2) (y1 y2))) 

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; Exercise 2.54
;(define (my-equal? a b)
;  (cond ((and (not (pair? a)) ..


        