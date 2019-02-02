#lang racket

(require "utilities.rkt")


(define assign #hash())

; Fill in your code here. Should finally define a function
; called dpll which returns true or false. Should additionally
; store the satisfying assignment in the variable assign.

(define (neg var)               ;;Negation : Converts Var to Not(Var) and vice versa
  (match var
    [(Var x) (Not (Var x))]
    [(Not (Var x)) (Var x)]))

(define (unit-checker exp )  ;;Checks whether there is a unit-clause present 
  (match exp
    [(or (Var x) (Not x)) exp]
    [(Or x y) #f]
    [(Const x) #f]
    [(And x y) (if (or (Var? x) (Not? x)) x (unit-checker y))]))

(define (or-neg exp var) ;;Removes the negation of the unit clause from the or clause
                         ;;Returns (Const #f) if only the negation is present
  (define (or-neg-h exp var)
  (cond [(or (Var? (Or-y exp)) (Not? (Or-y exp))) (if (equal? (Or-y exp) (neg var))
                                                      (if (equal? (Or-x exp) (neg var)) (Const #f) (Or-x exp))
                                                      (if (equal? (Or-x exp) (neg var)) (Or-y exp) exp))]
        [else (if (equal? (Or-x exp) (neg var)) (or-neg-h (Or-y exp) var) (Or (Or-x exp) (or-neg-h (Or-y exp) var)))]))
  ; (or-neg-h exp var))
 (const-remover (or-neg-h exp var)))

(define (const-remover exp) ;;Helper function in or-neg
  (cond [(or (Var? exp) (Not? exp)) exp]
        [(Const? exp) exp]
        [(Const? (Or-y exp)) (Or-x exp)]
        [(or (Var? (Or-y exp)) (Not? (Or-y exp))) exp]
        [else (Or (Or-x exp) (const-remover (Or-y exp)))]))

(define (or-check-var exp var) ;;Checks whether the given unit clause is present in the exp
  (match exp
    [(or (Var x) (Not x)) (if (equal? exp var) #t #f)]
    [(Or x y) (if (equal? x var) #t (or-check-var y var))]))
    
(define (elim exp var )  ;; Elimination- used for Unit Elimination as well as General Elimination
  (match exp
    [(or (Var x) (Not x))  (cond [(equal? exp var) (Const #t)]
                                 [(equal? exp (neg var)) (Const #f)]
                                 [else exp])]
    [(Or x y) (if (or-check-var exp var) (Const #t) (or-neg exp var)) ]
    [(Const x) exp]
    [(And x y) (And (elim x var) (elim y var))]))

(define (or-check-pol exp var)  ;;Checks whether the polarity of the variable is same in or-clause
  (match exp
    [(or (Var x) (Not x)) (if (equal? exp (neg var)) #f #t)]
    [(Or x y) (if (equal? x (neg var)) #f (or-check-pol y var))]))

(define (polarity-checker exp var) ;;Checks whether the polarity of the variable is same in expression
  (match exp
    [(or (Var x) (Not x)) (if (equal? exp (neg var)) #f #t)]
    [(Or x y) (or-check-pol exp var)]
    [(Const x) #t]
    [(And x y) (and (polarity-checker x var) (polarity-checker y var))]))

(define (literal-elim exp var)  ;;Literal Elimination: Could have also used general elimination but it takes less time
  (match exp
    [(or (Var x ) (Not x)) (Const #t)]
    [(Const x) exp]
    [(Or x y) (if (or-check-var exp var) (Const #t) exp)]
    [(And x y) (And (literal-elim x var) (literal-elim y var))]))

(define (polar-var exp)  ;;Checks if there is a variable which has the same polarity in the expression
                         ;;Can also use general elimination but it takes less time
  (define (polar-var-help expi)
    (match expi
      [(or (Var x) (Not x)) (if (polarity-checker exp expi) expi #f)]
      [(Const x) #f]
      [(Or x y) (cond [(or (Var? y) (Not? y)) (if (polarity-checker exp x) x
                                                  (if (polarity-checker exp y) y #f))]
                      [else (if (polarity-checker exp x) x (polar-var-help y))])]
      [(And x y) (let* ([r (polar-var-help x)])
                   (if (false? r) (polar-var-help y) r))]))

  (polar-var-help exp))

(define (evaluator exp)  ;;Evaluates the value of the expression
  (define (one-false exp)
    (match exp
      ((or (Var x) (Not x) ) #f)
      ((Or x y) #f)
      ((Const x) (not x))
      ((And x y) (or (one-false x) (one-false y)))))

  (define (all-true exp)
    (match exp
      ((or (Var x) (Not x) ) #f)
      ((Or x y) #f)
      ((Const x) x)
      ((And x y) (and (all-true x) (all-true y)))))

  (cond [(one-false exp) #f]
        [(all-true exp) #t]
        [else 0]))

(define (first-lit exp) ;;Finds out the first literal of the expression
  (match exp
    [(or (Var x) (Not x)) exp]
    [(Or x y) x]
    [(Const x) #f]
    [(And x y) (if (false? (first-lit x)) (first-lit y) (first-lit x))]))

(define (dpll exp)      ;;The main function that evaluates the expression
  (set! assign #hash())
  (dpll-helper exp ))

(define (dpll-helper exp )   ;;The helper fuction of the above function
  (let* ([e (evaluator exp)])
      (cond [(equal? e #t) #t ]
            [(false? e) #f]
         [(= 0 e) (cond [(unit-checker exp)
                           ( let* ( [uexp (unit-checker exp)]
                                                   [en (elim exp uexp)]
                                                   [m (dpll-helper en )])
                                              (if m (if (Var? uexp) (begin (set! assign (dict-set assign (Var-lit uexp) #t)) #t)
                                                        (begin (set! assign (dict-set assign (Var-lit (Not-e uexp)) #f)) #t))
                                                  #f))]

                        [(polar-var exp)
                              ( let* ([pexp (polar-var exp)]
                                                    [en (literal-elim exp pexp)]
                                                    [m (dpll-helper en )])
                                               (if m (if (Var? pexp) (begin (set! assign (dict-set assign (Var-lit pexp) #t)) #t)
                                                         (begin (set! assign (dict-set assign (Var-lit (Not-e pexp)) #f)) #t))
                                                   #f))]

                         [else (let* ([l (first-lit exp)])

                                 (if (Var? l) (let* ([en1 (elim exp l)]
                                                     [m1 (dpll-helper en1  )])
                                                                (if m1 (begin (set! assign (dict-set assign (Var-lit l) #t)) #t)
                                                                    (let* ([en2 (elim exp (neg l))]
                                                                           [m2 (dpll-helper en2 )])
                                                                      (if m2 (begin (set! assign (dict-set assign (Var-lit l) #f)) #t) #f))))
                                                              
                                                              (let* ([en1 (elim exp l)]
                                                                     [m1 (dpll-helper en1)])
                                                                (if m1 (begin (set! assign (dict-set assign (Var-lit (Not-e l)) #f)) #t)
                                                                    (let* ([en2 (elim exp (neg l))]
                                                                           [m2 (dpll-helper en2 )])
                                                                      (if m2 (begin (set! assign (dict-set assign (Var-lit (Not-e l)) #t)) #t) #f))))))])])))






