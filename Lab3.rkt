#lang typed/racket

(require typed/rackunit)

;; Returns true for s-expressions that are a list containing a number, the symbol 'chris, and a symbol
(define (parse000 [e : Sexp]) : Boolean
  (match e
    [(list (? number? n) 'chris (? symbol? s)) #t]
    [other #f]))

(check-equal? (parse000 '(1 chris a)) #t)
(check-equal? (parse000 'a) #f)
(check-equal? (parse000 '(a chris a)) #f)
(check-equal? (parse000 '(2 chriss a)) #f)

;; Returns the symbol from a list containing a number, 'chris, and a symbol, #f otherwise
(define (parse001 [e : Sexp]) : (U Symbol Boolean)
  (match e
    [(list (? number? n) 'chris (? symbol? s)) s]
    [other #f]))

(check-equal? (parse001 '(1 chris a)) 'a)
(check-equal? (parse001 'a) #f)
(check-equal? (parse001 '(a chris a)) #f)
(check-equal? (parse001 '(2 chriss a)) #f)

;; Returns the list of numbers from s-expressions that are lists of length three
;; whose second element is a list of real numbers
(define (parse002 [e : Sexp]) : (U (Listof Real) Boolean)
  (match e
    [(list any1 (list (? real? nums) ...) any2) (cast nums (Listof Real))]
    [other #f]))

(check-equal? (parse002 '(1 (1 2 3) a)) (list 1 2 3))
(check-equal? (parse002 'a) #f)
(check-equal? (parse002 '(a chris a)) #f)
(check-equal? (parse002 '(2 chriss a)) #f)


;; Returns the symbol 'okay if the value passed to the function is a number, returns
;; an error otherwise
(define (ohno [e : Any]) : Symbol
  (cond
    [(number? e) 'okay]
    [else (error 'ohno "expected a number, got ~e" e)])
  )

(check-exn (regexp (regexp-quote "expected a number"))
           (lambda ()(ohno "tornado")))

(check-exn (regexp (regexp-quote "expected a number"))
           (lambda ()(ohno 'hail)))

(check-equal? (ohno 4) 'okay)
(check-equal? (ohno 5.4) 'okay)



