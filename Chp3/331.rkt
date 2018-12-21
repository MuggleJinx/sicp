#lang sicp

; Ex 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; Ex 3.13
(define (make-circle x)
  (set-cdr! (last-pair x) x)
  x)

; Ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; Equal?  Share?

(define x '(a b))
(define z1 (cons x x))

; Ex 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; Ex 3.17

; Ex 3.18 
(define (any? p xs)
  (cond ((null? xs) #f)
        ((p (car xs)) #t)
        (else (any? p (cdr xs)))))
      
(define (circle? xs)
  (let ((ls '()))
    (define (check xs ls)
      (cond ((null? xs) #f)
            ((any? (lambda (x) (eq? x xs)) ls) #t)
            (else (check (cdr xs) (cons xs ls)))))
    (check xs ls)))
    
;-------------------------------------------------
(define (cons-1 x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "" m))))
  dispatch)
(define (car-1 z) (z 'car))
(define (cdr-1 z) (z 'cdr))

(define (cons-2 x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m



