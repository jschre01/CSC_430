#lang typed/racket
(require typed/rackunit)
(+ 5 4)
(check-equal? (* 4 13) 52)
(check-equal? (or false true) true)
'cheese

; add two numbers together
(define (add-nums a b)
  (+ a b))
(check-equal? (add-nums 5 6) 11)

;Section 4.1 Excercise 15
;Returns true if sunny is false or friday is true
(define (==> sunny friday)
  (cond
    [(equal? false sunny)
     true]
    [(equal? true friday)
     true]
    [else false]))
  
(check-equal? (==> false false) true)
(check-equal? (==> true false) false)
(check-equal? (==> false true) true)
(check-equal? (==> true true) true)

;Section 4.1 Excercise 19 
;Replaces a specified index of a string with a "_"
(define (string-insert [str : String] [i : Integer]): String
  (define len (string-length str))
  (define str1 (substring str 0 i))
  (define str2 (substring str i))
  (string-append str1 "_" str2)
  )

(check-equal? (string-insert "bill" 2) "bi_ll")
(check-equal? (string-insert "Cheester" 4) "Chee_ster")
(check-equal? (string-insert "2123" 0) "_2123")
(check-equal? (string-insert "billy" 5) "billy_")

;Section 4.1 Excercise 27
;Computes expected number of attendees, revenue, cost, and profit based on ticket price
(: people Integer)
(define people 120)
(: price Real)
(define price 5.0)
(: change Integer)
(define change 15)
(: incr Real)
(define incr 0.1)
(: fixed Integer)
(define fixed 180)
(: var Real)
(define var 0.04)
(define (attendees [ticket-price : Real]): Real
  (- people (* (- ticket-price price) (/ change incr))))

(define (revenue [ticket-price : Real]): Real
  (* ticket-price (attendees ticket-price)))

(define (cost [ticket-price : Real]): Real
  (+ fixed (* var (attendees ticket-price))))

(define (profit [ticket-price : Real]): Real
  (- (revenue ticket-price)
     (cost ticket-price)))

(check-equal? (attendees 5) 120.0)
(check-equal? (revenue 5) 600.0)
(check-equal? (cost 5) 184.8)
(check-equal? (profit 5) 415.2)


;Section 4.2
;Function takes in a deposit amount, and returns the interest amount that is made in a year
(define (interest [deposit : Real]): Real
  (cond
    [(<= deposit 1000)
     (* deposit .04)]
    [(and (> deposit 1000)(<= deposit 5000))
     (* deposit .045)]
    [else (* deposit .05)]))

(check-equal? (interest 1) .04)
(check-equal? (interest 1000) 40.0)
(check-equal? (interest 2000) 90.0)
(check-equal? (interest 5000) 225.0)
(check-equal? (interest 10000) 500.0)


;Section 4.4
(define-type furniture (U desk bookshelf))
(struct desk ([width : Real]
             [height : Real]
             [depth : Real])
  #:transparent)
(struct bookshelf ([depth : Real]
                   [shelves : Integer]
                   [width : Real])
  #:transparent)

(define my-bookshelf
  (bookshelf 5.0 12 4.0))

(define my-desk
  (desk 10.0 20.0 10.0))
; Determines the floor space that the furniture takes up 
(define (furniture-footprint [f : furniture]) : Real
 (match f
   [(desk wid hgt dep) (* wid dep)]
   [(bookshelf w s d) (* w d)]
   ))
    
  

(check-equal? (furniture-footprint my-desk) 100.0)
(check-equal? (furniture-footprint my-bookshelf) 20.0)


