#lang typed/racket

(require typed/rackunit)

;;ExprC is the type for expressions in DXUQ2 language
;;ExprC can be a NumC (number), BinC (binary expression), idc (identifier), or appC (application)
(define-type ExprC (U NumC BinC idC appC))
(struct NumC ([n : Number])
  #:transparent)
(struct BinC ([o : Symbol][l : ExprC][r : ExprC])
  #:transparent)
(struct idC ([s : Symbol])
  #:transparent)
(struct appC ([fun : Symbol][arg : ExprC])
  #:transparent)


;;FunDefC is the type for defining new functions in DXUQ2 language
(define-type FunDefC (U fdc))
(struct fdc ([name : Symbol][arg : Symbol][body : ExprC])
  #:transparent)

;Helper functions to access members of FunDefC
;(define (fdc-arg [fdc : FunDefC]): symbol
;  (match fdc
;    [(FunDefC n a b) a])
;  )

;(define (fdc-body [fdc : FunDefC]): ExprC
;  (match fdc
;    [(FunDefC n a b) b])
;  )

;(define (fdc-name [fdc : FunDefC]): symbol
;  (match fdc
;    [(FunDefC n a b) n])
;  )


;;lookup takes in a symbol representing a binary operator, and its right and left arguments,
;;and computes the expression with the correct operator, returning a number
(define (lookup [o : Symbol][l : ExprC][r : ExprC][fds : (Listof FunDefC)]): Number
  (cond
  [(equal? o '+) (+ (interp l fds) (interp r fds))]
  [(equal? o '-) (- (interp l fds) (interp r fds))]
  [(equal? o '*) (* (interp l fds) (interp r fds))]
  [(equal? o '/) (/ (interp l fds) (interp r fds))]
  [else 0]
  ))


;;TODO : Test cases for lookup function



;;interp takes in an ExprC and a list of FunDefC, and evaluates the expression or functions
(define (interp [arith : ExprC][fds : (Listof FunDefC)]) : Number
  (match arith
    [(NumC n) n]
    [(BinC o l r)(lookup o l r fds)]
    [(appC f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst (interp a fds)
                                 (fdc-arg fd)
                                 (fdc-body fd))
                          fds))]
    [(idC s)(error 'interp "shouldn't get here DXUQ")])
  )

;;Test Cases for interp method
(check-equal? (interp (BinC '+ (NumC 5) (NumC 10)) '()) 15)
(check-equal? (interp (BinC '- (NumC 15) (NumC 3)) '()) 12)
(check-equal? (interp (BinC '* (NumC 3) (NumC 5)) '()) 15)
(check-equal? (interp (BinC '/ (NumC 6) (NumC 2)) '()) 3)
(check-equal? (interp (BinC '+ (BinC '* (NumC 4.0) (NumC 2.5)) (BinC '- (NumC 13) (NumC 5.5))) '()) 17.5)


;;Takes in an in Expression, sub value, and sub expressions and return a new expression with \
;;each instance of the sub expression converted to the value in the in expression 
(define (subst [what : Number][for : Symbol][in : ExprC]): ExprC
  (match in
    [(NumC n) in]
    [(BinC o l r) (BinC o (subst what for l) (subst what for r))]
    [(idC s) (cond
               [(equal? s for) (NumC what)]
               [else in])]
    [(appC f a)(appC f (subst what for a))])
  )

;;TODO: Test Cases for subst 



;;get-fundef takes in a list of functions and a symbol and returns the function that
;;matches the symbol
(define (get-fundef [n : Symbol][fds : (Listof FunDefC)]): FunDefC
  (match fds
    [(cons f r)(cond
                 [(equal? n (fdc-name f)) f]
                 [else (get-fundef n r)])]
    ['() (error 'get-fundef "reference to undefined function DXUQ")])
  )

;;TO: Test Cases for get-fundef
