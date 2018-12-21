#lang sicp

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient fund"))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "error" m))))
  dispatch)
  
;; Test
(define acc (make-account 100))

;; Ex 3.1 Accumulator
(define (make-accumulator n)
  (lambda (x)
    (begin (set! n (+ x n))
           n)))

;; Ex 3.2
(define (make-monitored f)
  (let ((n 0))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          n
          (begin (set! n (+ n 1))
                 (f x))))))
;; Ex 3.3
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

;; Ex 3.4
(define (make-acc2 balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

;  (define (nope x) "Incorrect passwd")
;  (define mnope (make-monitored nope))

  (define mnope
    (let ((nope (lambda (x) "Incorrect pd")))
      (make-monitored nope)))

  (define (call-the-police x)
    "youre doomed")
  (define (dispatch p m)
    (cond ((> (mnope 'how-many-calls?) 3) call-the-police)
          ((not (eq? passwd p)) mnope)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "error" m))))
  dispatch)

(define (new x1 x2)
  (define a 1)
  (define b a)
  x1)
