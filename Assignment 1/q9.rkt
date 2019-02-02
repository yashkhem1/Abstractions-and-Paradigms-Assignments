#lang racket
(define (num-adder a n l)
  (cond [(= n (length l)) l]
        [else (num-adder a n (cons a l))]))

(define (u2b n x)
  (num-adder 0 n (u2b-helper n x '())))

(define (u2b-helper n x l)
  (cond  ;[(and (= n 0) (not (= x 0))) "Out of Range"]
         [(> x (- (expt 2 n) 1)) "Out Of Range"]
         [(= x 0) l]
         [(= 0 (remainder x 2))  (u2b-helper (- n 1) (quotient x 2) (append  (list 0) l))]
         [else (u2b-helper (- n 1) (quotient x 2) (append  (list 1) l)) ]))

(define (fix2d n a)
  (cond [(null? a) 0]
        [else (+ (* (expt 2 (- n 1)) (car a)) (fix2d (- n 1) (cdr a)))]))

(define (d2fix n a)
  (append (u2b n (floor a)) (d2b n (- a (floor a)))))

(define (d2b n d)
  (cond [(= 0 n) '()]
        [(< (* 2 d) 1) (cons 0 (d2b (- n 1) (* 2 d)))]
        [else (cons 1 (d2b (- n 1) (- (* 2 d) 1)))]))