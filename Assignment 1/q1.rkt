#lang racket
(define (num-adder a n l)
  (cond [(= n (length l)) l]
        [else (num-adder a n (cons a l))]))

(define (b2u n a)
  (b2u-helper  0 a))

(define (b2u-helper  x  a)
  (cond [(null? a ) x]
        [(= 1 (car a)) (b2u-helper (+ (* 2 x) 1) (cdr a))]
        [else (b2u-helper (* 2 x) (cdr a))]))

(define (u2b n x)
  (num-adder 0 n (u2b-helper n x '())))

(define (u2b-helper n x l)
  (cond  ;[(and (= n 0) (not (= x 0))) "Out of Range"]
         [(> x (- (expt 2 n) 1)) "Out Of Range"]
         [(= x 0) l]
         [(= 0 (remainder x 2))  (u2b-helper (- n 1) (quotient x 2) (append  (list 0) l))]
         [else (u2b-helper (- n 1) (quotient x 2) (append  (list 1) l)) ]))