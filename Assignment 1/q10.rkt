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
(define (d2b n d)
  (cond [(= 0 n) '()]
        [(< (* 2 d) 1) (cons 0 (d2b (- n 1) (* 2 d)))]
        [else (cons 1 (d2b (- n 1) (- (* 2 d) 1)))]))
(define (u2b n x)
  (num-adder 0 n (u2b-helper n x '())))

(define (u2b-helper n x l)
  (cond  ;[(and (= n 0) (not (= x 0))) "Out of Range"]
         [(> x (- (expt 2 n) 1)) "Out Of Range"]
         [(= x 0) l]
         [(= 0 (remainder x 2))  (u2b-helper (- n 1) (quotient x 2) (append  (list 0) l))]
         [else (u2b-helper (- n 1) (quotient x 2) (append  (list 1) l)) ]))


;float2d
(define (concat a m n)
  (cond [(= n 0) '()]
        [(= m 1) (cons (car a) (concat (cdr a) m (- n 1)))]
        [else (concat (cdr a)  (- m 1) (- n 1))]))


(define (float2d k l a)
  (float-helper k l (car a) (cadr a) (caddr a) (-  (expt 2 (- k 1)) 1)))

(define (f-calculator lk c)
  (cond [(null? lk) 0]
        [else (+ (* (expt 2 c) (car lk)) (f-calculator (cdr lk) (- c 1)))]))

(define (float-helper k l s lk ll bias)
  (cond [(and (equal? lk (make-list k 0)) (equal? ll (make-list l 0)))
         (cond [(= s 0) +0.0]
               [else   -0.0])]
        [(equal? lk (make-list k 0))
         (cond [(= s 0) (*  (f-calculator ll -1) (expt 2 (-  1 bias)))]
               [else  (* -1 (*  (f-calculator ll -1) (expt 2 (-  1 bias))))])]
        [(and (equal? lk (make-list k 1)) (equal? ll (make-list l 0)))
         (cond [(= s 0) +inf.0]
               [else -inf.0])]
        [(equal? lk (make-list k 1)) "NaN"]
        [else (cond [(= s 0) (* (+ 1 (f-calculator ll -1)) (expt 2 (- (b2u k lk) bias)))]
                    [else (* -1 (* (+ 1 (f-calculator ll -1)) (expt 2 (- (b2u k lk) bias))))])]))

;d2float

(define (u2b-min x)
    (cond [(= x 0) '(0)]
          [else (u2b-min-help x)]))

(define (u2b-min-help x)
  (cond [(= x 0) '()]
        [(= 1 (remainder x 2)) (append (u2b-min-help (quotient x 2)) (list 1))]
        [else (append (u2b-min-help (quotient x 2)) (list 0))]))
                 

(define (min-val k l )
   (cons 0 (list (make-list k 0) (append (make-list (- l 1) 0) (list 1)))))

(define (max-val k l)
  (cons 0  (list (append (make-list (- k 1) 1) (list 0)) (make-list l 1))))

(define (d2float k l x)
(cond [(>= x 0) (d2float-helper k l x (u2b-min (floor x)) (- x (floor x)) (- (expt 2 (- k 1)) 1))]
      [else (cons 1 (cdr(d2float k l (* -1 x))))]))

(define (d2float-helper k l x l1 d bias)
  (cond [(>= x (float2d k l (max-val k l))) (max-val k l)]
        [ (< x (float2d k l (min-val k l))) (cons 0 (list (make-list k 0) (make-list l 0)))]
        [(and (> x 0) (equal? '(0) l1)) (cons 0 (d2-help-1 k l bias  0 (d2b (+ bias (- l 1)) d)))]
        [else (cons 0 (d2-help-2 k l bias l1   (d2b  (-(+ bias (- l 1)) (- (length l1) 1)) d))) ])) 



(define (d2-help-1 k l bias c l1 )
  (cond [(= c (- 1 bias )) (list (make-list k 0) l1)]
        [(equal? 1 (car l1)) (list (u2b k (+ bias (- c 1))) (concat l1 2 (+ l 1)))]
        [else (d2-help-1 k l bias  (- c 1) (cdr l1))]))

(define (d2-help-2 k l bias l1   l2)
  (list (u2b k (+ bias (- (length l1) 1))) (concat (append (cdr l1) l2) 1 l)))

