#lang sicp
;;
(define (square a) (* a a))

;; Ben with orthogonal coordinate
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))

;; Alyssa with polar
;; ...

;; Use
;; (make-from-real-imag (real-part z) (imag-part z))


;;; 2.4.2
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum --TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "... --CONTENTS" datum)))
