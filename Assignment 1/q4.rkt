#lang racket
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