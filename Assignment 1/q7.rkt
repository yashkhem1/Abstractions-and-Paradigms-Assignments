#lang racket
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

(define (invert n a)
  (cond [(null? a) a]
        [(equal? 0 (car a)) (cons 1 (invert (- n 1) (cdr a)))]
        [else (cons 0 (invert (- n 1) (cdr a)))]))

(define (u-inverse n a)
  (u-add n (invert n a) (append (make-list (- n 1) 0) (list 1))))
                            

(define (check-big n a b)
  (cond [(= 0 n) #t]
        [(< (car a) (car b)) #f]
        [(> (car a) (car b)) #t]
        [else (check-big (- n 1) (cdr a) (cdr b))]))

(define (u-sub n a b)
  (cond [(not (check-big n a b)) 0]
        [else (u-add n a (u-inverse n b))]))

(define (u-div n x y)
  #|(cond [(equal? 0 (u-sub n x y)) (list (make-list  n 0) (shift-l 0  n x 0))]
        [else (let* ([d (u-div n (u-sub n x y) y)])
                (list (u-add  n (car d) (shift-l 0  n '(1) 0)) (cadr d)))]))|#
 (define (u-div-help x y c r)
   (cond [(and (equal? 0 (u-sub n (shift-l 0 n c 0) y)) (null? x)) (list (shift-l 0 n (append r (list 0)) 0) (shift-l 0 n c 0))]
          [(null? x) (list (shift-l 0 n (append r (list 1)) 0) (u-sub n (shift-l 0 n c 0) y))]
          [(equal? 0 (u-sub n (shift-l 0 n c 0) y)) (u-div-help (cdr x) y (append c (list (car x))) (append r (list 0)))]
          [else (u-div-help (cdr x) y (cdr (append (u-sub n (shift-l 0 n c 0) y) (list(car x)))) (append r (list 1)))]))

  (u-div-help (cdr x) y (list(car x)) '()))