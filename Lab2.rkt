#lang typed/racket

(require typed/rackunit)


;#1 

;list definitions for test cases
(define list1
  (list "Cheese" "Sort" "Chicken"))

(define list2
  (list "Tornado" "run" "reverand" "dodgeball"))

(define list3
  (list "happy" "feet"))


;rev-str-app takes in a list of strings, and returns a string that combines the
;input strings in reverse order
(define (rev-str-app [str_l : (Listof String)]): String
  (match str_l
    [(cons f r) (string-append (rev-str-app r) f)]
    ['() ""]
    ))



;Test Cases
(check-equal? (rev-str-app list1) "ChickenSortCheese")
(check-equal? (rev-str-app list2) "dodgeballreverandrunTornado")
(check-equal? (rev-str-app list3) "feethappy")


;#2
;rev-str-app is of type String. This makes sense because that is what I
;initialized the function to return. The type of '+' is extremelely lengthy
;so I am not going to paste it here. But it includes every possible kind
;of number. This makes sense because addition should be able to work
;with every number possible.



;#3

;Type and struct data definitions for Bicycles

(define-type bicycle (U Trek Bianchi Gunnar))

(struct Trek ([gears : Integer])
  #:transparent)

(struct Bianchi ([wheels : Integer])
  #:transparent)

(struct Gunnar ([speed : Integer])
  #:transparent)


;Data for bicycle structs

(define my-trek1
  (Trek 5))

(define my-bianchi1
  (Bianchi 6))

(define my-gunnar
  (Gunnar 4))

(define l1
  (list my-trek1 my-bianchi1 my-gunnar))

(define l2
  (list my-trek1 my-bianchi1 my-gunnar (Trek 7) (Trek 10) (Bianchi 11) (Gunnar 12)))


;#4

;onlyTreks takes in a list of bicycles and returns a list containing only treks
(define (onlyTreks [l : (Listof bicycle)]): (Listof bicycle)
  (match l
    [(cons f r)
     (cond
       [(Trek? f) (cons f (onlyTreks r))]
       [else (onlyTreks r)])]
    ['() '()]
  ))


;Test Cases
(check-equal? (onlyTreks l1) (list (Trek 5)))
(check-equal? (onlyTreks l2) (list (Trek 5) (Trek 7) (Trek 10)))



;#5

;onlyBianchis takes in a list of bicycles and returns a list
;containing only bianchies
(define (onlyBianchis [l : (Listof bicycle)]): (Listof bicycle)
  (match l
    [(cons f r)
     (cond
       [(Bianchi? f)(cons f (onlyBianchis r))]
       [else (onlyBianchis r)])]
    ['() '()]
    ))

;Test Cases
(check-equal? (onlyBianchis l1) (list (Bianchi 6)))
(check-equal? (onlyBianchis l2) (list (Bianchi 6) (Bianchi 11)))


;#6

;onlyThese takes in a list of bicycles and a predicate f and returns a list of
;only the bicycles in the list that satisfy the predicate
(define (onlyThese [l : (Listof bicycle)][F : (-> Any Boolean)]): (Listof bicycle)
  (match l
    [(cons f r)
     (cond
       [(F f)(cons f (onlyThese r F))]
       [else (onlyThese r F)])]
    ['() '()]
    ))


;Test Cases
(check-equal? (onlyThese l1 Trek?) (list (Trek 5)))
(check-equal? (onlyThese l1 Bianchi?) (list (Bianchi 6)))
(check-equal? (onlyThese l1 Gunnar?) (list (Gunnar 4)))
(check-equal? (onlyThese l2 Gunnar?) (list (Gunnar 4)(Gunnar 12)))


;#7

;Data Initialization
(define l3
  (list "a" "b" "c"))

(define l4
  (list 1 2 3))

(define l5
  (list 'd 'e 'f))

(define l6
  (list #f #f #t))

(define l7
  '())



;my-append takes in two lists, and returns a list of the second list appended
;to the first
(define (my-append [l1 : (Listof Any)][l2 : (Listof Any)]): (Listof Any)
  (match l1
    [(cons f r) (cons f (my-append r l2))]
    ['() l2]
  ))


;Test Cases
(check-equal? (my-append l3 l4) (list "a" "b" "c" 1 2 3))
(check-equal? (my-append l4 l5) (list 1 2 3 'd 'e 'f))
(check-equal? (my-append l5 l6) (list 'd 'e 'f #f #f #t))
(check-equal? (my-append l6 l3) (list #f #f #t "a"  "b" "c"))
(check-equal? (my-append l4 l7)(list 1 2 3))
(check-equal? (my-append l7 l4) (list 1 2 3))



;#8

;my-take takes in a list and and integer n and returns the first n elements
;of the list, or the whole list if the length is less than n
(define (my-take [l : (Listof Any)][n : Integer]) : (Listof Any)
  (match l
    [(cons f r)
     (cond
       [(> n 0) (cons f (my-take r (- n 1)))]
       [else '()])]
    ['() '()])
  )



;Test Cases
(check-equal? (my-take l3 2) (list "a" "b"))
(check-equal? (my-take l4 0) '())
(check-equal? (my-take l5 1) (list 'd))
(check-equal? (my-take l6 4) (list #f #f #t))