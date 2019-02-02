#lang racket
(define (invert n a)
  (cond [(null? a) a]
        [(equal? 0 (car a)) (cons 1 (invert (- n 1) (cdr a)))]
        [else (cons 0 (invert (- n 1) (cdr a)))]))

(define (u-inverse n a)
  (u-add n (invert n a) (append (make-list (- n 1) 0) (list 1))))


(define (num-adder a n l)
  (cond [(= n (length l)) l]
        [else (num-adder a n (cons a l))]))

(define (shift-l a n l m)
  (num-adder a n (append l (make-list m 0))))



(define (carry x y c)
  (cond [(= (+ x y c) 0) 0]
        [(= (+ x y c) 1) 0]
        [(= (+ x y c) 2) 1]
        [(= (+ x y c) 3) 1]))

(define (unit x y c)
  (cond [(= (+ x y c) 0) 0]
        [(= (+ x y c) 1) 1]
        [(= (+ x y c) 2) 0]
        [(= (+ x y c) 3) 1]))

  
(define (u-add n a b)
(cdr (u-add-help n a b )))

(define (u-add-help n a b)
    (cond  [(null? a) '(0)]
           [else (let* ([p (u-add-help (- n 1) (cdr a) (cdr b))])
                   (append (list(carry (car a) (car b) (car p))) (list(unit (car a) (car b) (car p))) (cdr p)))]))

(define (u-mult  n a b )
  (cond [(null? a) (make-list  (* 2 n)  0)]
        [(= 1 (car a)) (u-add (* 2 n) (shift-l  0  (* 2 n) b (length (cdr a))) (u-mult n (cdr a) b ))]
        [else (u-mult n (cdr a) b)]))

(define (s-mult n a  b)
  (cond [(= (+ (car a) ( car b)) 0) (u-mult n a b)]
        [(and (= (car a) 1) (= (car b) 0)) (s-mult-1 n b a)]
        [(and (= (car a) 0) (= (car b) 1)) (s-mult-1 n a b)]
        [else (s-mult-2 n a b)]))



(define (s-mult-1 n a b)
  (cond [(null? a) (make-list  (* 2 n)  0)]
        [(= 1 (car a)) (u-add (* 2 n) (shift-l  1  (* 2 n)  b (length (cdr a))) (s-mult-1 n (cdr a) b ))]
        [else (s-mult-1 n (cdr a) b)]))

(define (s-mult-2 n a b)
  (cond [(null? a) (make-list (* 2 n) 0)]
        [(let* ([u (s-mult-2 n (cdr a) b)])
           (cond [(= n (length a)) (u-add (* 2 n) (u-inverse (* 2 n) (shift-l  1  (* 2 n)  b (length (cdr a)))) u)]
                 [(= 1 (car a)) (u-add (* 2 n) (shift-l  1  (* 2 n)  b (length (cdr a))) u)]
                 [else u]))]))