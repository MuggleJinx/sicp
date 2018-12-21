#lang sicp

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (factorial2 n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))
;; Ex 3.7  <- Ex 3.3
(define (make-acc balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (nope x)
    "Incorrect passwd")
  (define (dispatch p m)
    (cond ((not (eq? passwd p)) nope)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "error" m))))
  dispatch)

(define peter-acc (make-acc 100 '2103))

(define (make-joint acc old-pw new-pw)
  (if (number? ((acc old-pw 'withdraw) 0))
      (lambda (pw op)
        (if (eq? pw new-pw)
            (acc old-pw op)
            (error "passwd worong")))
      (error "Fail to joint")))

;; Ex 3.8
(define (f-wrong x)
  (let ((a 0))
    (if (= a 0)
        (begin (set! a 1)
               x)
        0)))

(define f-right
  (let ((a 0))
    (lambda (x)
      (if (= a 0)
          (begin (set! a 1)
                 x)
          0))))

  
;
(define (make-acc2 balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (define balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (nope x)
    "Incorrect passwd")
  (define (dispatch p m)
    (cond ((not (eq? passwd p)) nope)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "error" m))))
  dispatch)
   
  