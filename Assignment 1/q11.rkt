#lang racket
(define (num-adder a n l)
  (cond [(= n (length l)) l]
        [else (num-adder a n (cons a l))]))

(define (shift-l a n l m)
  (num-adder a n (append l (make-list m 0))))

(define (concat a m n)
  (cond [(= n 0) '()]
        [(= m 1) (cons (car a) (concat (cdr a) m (- n 1)))]
        [else (concat (cdr a)  (- m 1) (- n 1))]))

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

(define (min-val k l )
   (cons 0 (list (make-list k 0) (append (make-list (- l 1) 0) (list 1)))))

(define (max-val k l)
  (cons 0  (list (append (make-list (- k 1) 1) (list 0)) (make-list l 1))))


;mult
(define (addb c n b)
  (cond [(= c 0 ) b]
        [else (u-add n (addb (- c 1) n b) (append (make-list (- n 1) 0) (list 1)))]))

(define (subb c n b)
  (cond [(= c 0 ) b]
        [else (u-sub n (subb (- c 1) n b) (append (make-list (- n 1) 0) (list 1)))]))

(define (mult-normal k l f1 f2)
  (let*  ([s (u-mult (+ l 1) (cons 1 (caddr f1)) (cons 1 (caddr f2)))]
          [s1 (u-mult (+ l 1) (cons 1 (caddr f1)) (cons 0 (caddr f2)))]
          [s2 (u-mult (+ l 1) (cons 0 (caddr f1)) (cons 1 (caddr f2)))]
          [s3 (u-mult (+ l 1) (cons 0 (caddr f1)) (cons 0 (caddr f2)))]  
          [e (u-add (* 2 k) (num-adder 0 (* 2 k) (cadr f1)) (num-adder 0 (* 2 k) (cadr f2)))]
          [e1 (u-add (* 2 k) (num-adder 0 (* 2 k) (cadr f1)) (append (make-list (- (* 2 k) 1) 0) (list 1)))]
          [e2 (u-add (* 2 k) (append (make-list (- (* 2 k) 1) 0) (list 1)) (num-adder 0 (* 2 k) (cadr f2)))]
          [e3 (u-add (* 2 k) (append (make-list (- (* 2 k) 1) 0) (list 1)) (append (make-list (- (* 2 k) 1) 0) (list 1)))]
          [bias (num-adder 0 (* 2 k) (append (list 0) (make-list (- k 1) 1) ))]
          ;[e-plus (u-add (* 2 k) e (append (make-list (- (* 2 k) 1) 0) (list 1)))]
          [lb (u-add (* 2 k) (num-adder 0 (* 2 k) (append (make-list (- k 1) 0) (list 1))) bias)]
          [ub (u-add (* 2 k) (num-adder 0 (* 2 k) (append (make-list (- k 1) 1) (list 0))) bias)])

   #| (cond [ (= 0 (car s))
            (cond [(equal? 0 (u-sub (* 2 k) e lb)) (cons 0 (list (make-list k 0) (make-list l 0)))]
                  [(equal? 0 (u-sub (* 2 k) ub e)) (max-val k l)]
                  [else (list (concat (u-sub (* 2 k) e bias) (+ 1 k) (* 2 k)) (concat (cddr s) 1 l) )])]
          [ (= 1 (car s))
            (cond [(equal? 0 (u-sub (* 2 k) e-plus lb)) ;(cons 0 (list (make-list k 0) (make-list l 0)))]
                   (check 
                  [(equal? 0 (u-sub (* 2 k) ub e-plus)) (max-val k l)]
                  [else (list (concat (u-sub (* 2 k) e-plus bias) (+ 1 k) (* 2 k)) (concat (cdr s) 1 l) )])])))|#
    (cond [(and (equal? (make-list k 0) (cadr f1)) (equal? (make-list k 0) (cadr f2))) (check-function k l s3 (addb 1 (* 2 k) e3) lb ub bias)]
          [(equal? (make-list k 0) (cadr f1)) (check-function k l s2 (addb 1 (* 2 k) e2) lb ub bias)]
          [(equal? (make-list k 0) (cadr f2)) (check-function k l s1 (addb 1 (* 2 k) e1) lb ub bias)]
          [else (check-function k l s (addb 1 (* 2 k) e) lb ub bias)])))
                                                                                       


(define (check-function k l s e lb ub bias)
  (cond [(= 1 (car s))
         (cond [(equal? 0 (u-sub (* 2 k) ub e)) (cdr(max-val k l))]
               [(equal? 0 (u-sub (* 2 k) e lb)) (check-function k l (cons 0 s) (addb 1 (* 2 k) e) lb ub bias)]
               [else (list (concat (u-sub (* 2 k) e bias) (+ k 1) (* 2 k)) (concat (cdr s) 1 l))])]
        [(= 0 (car s))
         (cond [(equal? 0 (u-sub (* 2 k) ub e)) (check-function k l (cdr s) (subb 1 (* 2 k) e) lb ub bias)]
               [(equal? e lb) (list (make-list k 0) (concat (cdr s) 1 l))]
               [(equal? 0 (u-sub (* 2 k) e lb)) (check-function k l (cons 0 s) (addb 1 (* 2 k) e) lb ub bias)]
               [else (check-function k l (cdr s) (subb 1 (* 2 k) e) lb ub bias)])]))

(define (mult k l f1 f2)
  (cond [(and (= (car f1) 0) (= (car f2) 0)) (cons 0 (mult-normal k l f1 f2))]
        [(= (car f1) 0) (cons 1 (mult-normal k l f1 f2))]
        [(= (car f2) 0) (cons 1 (mult-normal k l f1 f2))]
        [else (cons 0 (mult-normal k l f1 f2))]))

;Divide

