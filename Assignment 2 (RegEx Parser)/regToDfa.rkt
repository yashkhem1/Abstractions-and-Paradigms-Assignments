#lang racket
(require parser-tools/lex
         parser-tools/yacc)
(require "declarations.rkt")
(require "utilities.rkt")

(define (nullable t)
  (cond [(Epsilon? t) #t]
        [(Literal? t) #f]
        [(Or? t) (or (nullable (Or-t1 t)) (nullable (Or-t2 t)))]
        [(Then? t) (and (nullable (Then-t1 t)) (nullable (Then-t2 t)))]
        [(Star? t) #t]))

(define (buildNullable t)
  (cond [(Epsilon? t) (list(cons (Epsilon-n t) (nullable t)))]
        [(Literal? t) (list(cons (Literal-n t) (nullable t)))]
        [(Or? t) (append  (buildNullable (Or-t1 t))  (buildNullable (Or-t2 t)) (list (cons (Or-n t) (nullable t))))]
        [(Then? t) (append  (buildNullable (Then-t1 t)) (buildNullable (Then-t2 t)) (list(cons (Then-n t) (nullable t))))]
        [(Star? t) (append   (buildNullable (Star-t t)) (list (cons (Star-n t) (nullable t))))]))

;-------------------------------------------------------------------------------------------------------------------------------------------------

(define (firstpos t)
  (cond [(Epsilon? t) '()]
        [(Literal? t) (list (Literal-n t))]
        [(Or? t) (remove-duplicates (append (firstpos (Or-t1 t))   (firstpos (Or-t2 t))) )]
        [(Then? t) (cond [(nullable (Then-t1 t)) (remove-duplicates (append (firstpos (Then-t1 t))  (firstpos (Then-t2 t)) ))]
                         [else (firstpos (Then-t1 t))])]
        [(Star? t) (firstpos (Star-t t))]))



(define (buildFirst t)
   (cond [(Epsilon? t) (list(cons (Epsilon-n t) (firstpos t)))]
        [(Literal? t) (list(cons (Literal-n t) (firstpos t)))]
        [(Or? t) (append (buildFirst (Or-t1 t)) (buildFirst (Or-t2 t)) (list (cons (Or-n t) (firstpos t))))]                                        
        [(Then? t) (append (buildFirst (Then-t1 t)) (buildFirst (Then-t2 t)) (list (cons (Then-n t) (firstpos t))))]
        [(Star? t) (append (buildFirst (Star-t t)) (list (cons (Star-n t) (firstpos t))))]))

;-------------------------------------------------------------------------------------------------------------------------------------------------

(define (lastpos t)
  (cond [(Epsilon? t) '()]
        [(Literal? t) (list (Literal-n t))]
        [(Or? t) (remove-duplicates (append (lastpos (Or-t1 t))  (lastpos (Or-t2 t)) ))]
        [(Then? t) (cond [(nullable (Then-t2 t)) (remove-duplicates (append (lastpos (Then-t1 t))   (lastpos (Then-t2 t))))]
                         [else (lastpos (Then-t2 t))])]
        [(Star? t) (lastpos (Star-t t))]))


(define (buildLast t)
  (cond [(Epsilon? t) (list(cons (Epsilon-n t) (lastpos t)))]
        [(Literal? t) (list(cons (Literal-n t) (lastpos t)))]
        [(Or? t) (append (buildLast (Or-t1 t)) (buildLast(Or-t2 t)) (list (cons (Or-n t) (lastpos t))))]                                        
        [(Then? t) (append (buildLast (Then-t1 t)) (buildLast (Then-t2 t)) (list (cons (Then-n t) (lastpos t))))]
        [(Star? t) (append (buildLast (Star-t t)) (list (cons (Star-n t) (lastpos t))))]))

;-------------------------------------------------------------------------------------------------------------------------------------------------

(define (buildFollow t)
  (list-sorter (buildFollow-help t '())))

(define (buildFollow-help t l)
  (cond [(Literal? t) l]
        [(Epsilon? t) l]
        [(Then? t) (let* ([f (firstpos (Then-t2 t))]
                          [ln (map (lambda (x) (cons x f)) (lastpos (Then-t1 t))) ]
                          [lf (merger ln l )])
                       (merger (buildFollow-help (Then-t2 t) lf) (buildFollow-help (Then-t1 t) lf)))]
                     
                     
        [(Star? t)
                   (let* ([f (firstpos (Star-t t))]
                          [ln (map (lambda (x) (cons x f)) (lastpos (Star-t t))) ]
                          [lf (merger ln l )])
                      (buildFollow-help (Star-t t) lf))]
        
        [(Or? t) (merger  (buildFollow-help (Or-t2 t) l) (buildFollow-help (Or-t1 t) l))]))

(define (merger-help l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [(= (caar l1) (caar l2)) (cons (cons (caar l1)   (remove-duplicates (append (cdar l1) (cdar l2)))) (merger-help (cdr l1) (cdr l2)))]
        [(> (caar l1) (caar l2)) (cons (car l2) (merger-help l1 (cdr l2)))]
        [else (cons (car l1) (merger-help (cdr l1) l2))]))

(define (list-sorter l)
  (cond [(null? l) l]
        [else (cons (cons (caar l) (sort (cdar l) <)) (list-sorter (cdr l)))]))
        
(define (merger l1 l2)
  (merger-help (sort l1 list-compare) (sort l2 list-compare)))
  
(define (list-compare x1 x2)
  (< (car x1) (car x2)))

;----------------------------------------------------------------------------------------------------------------------------------------------

(define (same-sym l t)
  (same-sym-helper  (sort l <)  (literals t) '()))

(define (literals t )
  (cond [(Literal? t) (list(cons (Literal-n t) (list (Literal-c t))))]
        [(Epsilon? t) '()]
        [(Then? t) (append (literals (Then-t1 t)) (literals (Then-t2 t)))]
        [(Or? t) (append (literals (Or-t1 t)) (literals (Or-t2 t)))]
        [(Star? t) (literals (Star-t t))]))

(define (same-sym-helper l lf res)
  (cond [(null? l) res]
        [(= (car l) (caar lf)) (same-sym-helper (cdr l) (cdr lf) (joiner (car lf) res))]
        [else (same-sym-helper l (cdr lf) res)]))

(define (joiner l1 l2)
  (cond 
        [(null? l2) (list (append (cdr l1) (list (car l1))))]
        [(equal? (cadr l1) (caar l2)) (cons (cons (caar l2) (append (cdar l2) (list (car l1)))) (cdr l2))]
        [else (cons (car l2) (joiner l1 (cdr l2)))]))

(define (symbols-help l)
  (cond [(null? l) '()]
        [else (cons (caar l) (symbols-help (cdr l)))]))

(define (transitions-helper l lh t ft) 
  (cond [(null? lh) '()]
        [(equal? (caar lh) "#") (transitions-helper l (cdr lh) t ft)]
        [else (cons (Trans (sort l <)  (caar lh) (sort (follow (cdar lh) ft ft) <)) (transitions-helper l (cdr lh) t ft))]))

(define (follow l li lr)
  (cond [(null? l) '()]
        [(= (car l) (caar li)) (remove-duplicates (append (cdar li) (follow (cdr l) lr lr)))]
        [else  (follow l (cdr li) lr)]))

(define (follownode-help l)
  (cond [(null? l) '()]
        [else (cons (Trans-final (car l)) (follownode-help (cdr l)))]))

(define (red-help l1 l)
  (cond [(null? l) '()]
        [(equal? (caar l) "#") (list l1)]
        [else (red-help l1 (cdr l))]))

(define (in a l)
  (cond [(null? l) #f]
        [(equal? a (car l)) #t]
        [else (in a (cdr l))]))

(define (insert l node stack)
  (cond [(null? l) stack]
        [(not (or (in (car l) stack) (in (car l) node))) (insert (cdr l) node (append stack (list (car l))))]
        [else (insert (cdr l) node stack)]))

(define (graph-help t stack node trans redn sym)
  (cond [(null? stack) (Graph (firstpos t) node trans redn sym)]
        [else             
         (let* ( [s (same-sym (car stack) t) ]
                 [tr (transitions-helper (car stack) s t (buildFollow t))])
           (graph-help t (insert (follownode-help tr) (append node (list (car stack))) (cdr stack)) (remove-duplicates (append node (list (car stack))))
                     (append trans tr)  (remove-duplicates (append (red-help (car stack) s)  redn))
                     (remove-duplicates (append sym (symbols-help s)))))]))

(define (buildGraph reg)
  (graph-help (maketree reg) (list(firstpos (maketree reg))) '() '() '() '() ))


