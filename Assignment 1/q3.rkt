#lang racket
(define (num-adder a n l)
  (cond [(= n (length l)) l]
        [else (num-adder a n (cons a l))]))

(define (n2m n m a)
  (cond [(= 0 (car a)) (num-adder 0 m a)]
        [else (cons (car a) (num-adder 1 (- m 1) (cdr a)))]))