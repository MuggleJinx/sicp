#lang sicp
(#%require sicp-pict)

;; 2.2.4 

(define ei einstein)

(define ei2 (beside ei (flip-vert ei))) ; beside feels like append
(define ei4 (below ei2 ei2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define ei4_new (flipped-pairs ei))

; Recursion
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;; define square-limit

; higher-class operation
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flip-pairs-new painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit-new painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
          
; Exercise 2.45
(define (split op1 op2)
  (define (aux painter n)
    (if (= n 0)
        painter
        (let ((smaller (aux painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  aux)

(define right-split-new (split beside below))
(define up-split-new (split below beside))
; End 2.45

; Exercise 2.46
(define (make-v x y) (cons x y))
(define (xcor-v v) (car v))
(define (ycor-v v) (cdr v))
(define (add-v va vb)
  (make-v (+ (xcor-v va) (xcor-v vb))
          (+ (ycor-v va) (ycor-v vb))))
(define (sub-v va vb)
  (make-v (- (xcor-v va) (xcor-v vb))
          (- (ycor-v va) (ycor-v vb))))
(define (scale-v s v)
  (make-v (* s (xcor-v v))
          (* s (ycor-v v))))
; Exercise 2.47
(define (make-f origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-f frame) (car frame))
(define (edge1-f frame) (cadr frame))
(define (edge2-f frame) (caddr frame))
; End exercise

(define (frame-coord-map frame)
  (lambda (v)
    (add-v (origin-f frame)
           (add-v (scale-v (xcor-v v)
                           (edge1-f frame))
                  (scale-v (ycor-v v)
                           (edge2-f frame))))))

;; Painter  as a procedure









  