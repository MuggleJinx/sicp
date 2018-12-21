#lang sicp

;; Queue

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '())) 

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "empty" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "empty" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; Ex 3.21 -- Print Queue
(define (print-queue queue)
  (display (front-ptr queue)))

;; EX 3.22 -- make-queue
(define (make-queue-2)
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty?) (null? front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'front-queue)
             (if (empty?)
                 (error "empty")
                 (car front-ptr)))
            ((eq? m 'delete-queue!)
             (if (empty?)
                 (error "empty")
                 (set-front-ptr! (cdr front-ptr))))
            ((eq? m 'insert-queue!)
             (lambda (item)
               (let ((new-pair (cons item nil)))
                 (cond ((empty?)
                        (set-front-ptr! new-pair)
                        (set-rear-ptr! new-pair))
                       (else
                        (set-cdr! rear-ptr new-pair)
                        (set-rear-ptr! new-pair))))))
            ((eq? m 'print)
             (display front-ptr))))
;             front-queue)))
    dispatch))

(define (delete-queue-2! queue)
  (queue 'delete-queue!))

(define (insert-queue-2! queue item)
  ((queue 'insert-queue!) item))
                     
;; Ex 3.23 -- Deque
;; TO-DO

