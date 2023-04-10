#lang typed/racket

(require typed/rackunit)

;2.1 Problem 2.3.3

;constants ticket-price, performance-cost, and attendee-cost
(define ticket-price 5.0)
(define performance-cost 20.0)
(define attendee-cost 0.5)
;Total-profit calculates the total profit a movie theater makes based on # of attendees
(define (total-profit [attendees : Integer]): Real
  (- (* attendees ticket-price) (+ performance-cost (* attendees attendee-cost)))
  )

(check-= (total-profit 0) -20.0 .01 "You failed")
(check-= (total-profit 10) 25.0 .01 "You failed")
(check-= (total-profit 5) 2.5 .01 "You failed")
(check-= (total-profit 4) -2.0 .01 "You failed")
(check-= (total-profit 20) 70.0 .01 "You failed")

;2.1 Problem 3.3.3

;constant pi
(define pi 3.14159265359)

;area-cylinder computes the surface area of a cylinder based on its base disk radius and height
(define (area-cylinder [radius : Real][height : Real]) : Real
  (+ (* (* 2 pi) (* radius height)) (* (* 2 pi) (expt radius 2)))
  )

(check-= (area-cylinder 5 10) 471.24 .01 "You failed")
(check-= (area-cylinder 3 8) 207.35 .01 "You failed")
(check-= (area-cylinder 12 12) 1809.56 .01 "You failed")
(check-= (area-cylinder 0 0) 0 .01 "You failed")


;2.2 Magic Tricks


;Magic Trick Type Definition and Structures
(define-type magic-trick (U Card-Trick Guillotine))

(struct Card-Trick ([decks : Number]
                    [volunteers : Integer])
  #:transparent)

(struct Guillotine ([realism : Integer]
                    [has-tiger? : Boolean])
  #:transparent)


;trick-minutes takes in a magic-trick and returns how long that trick takes to perform
(define (trick-minutes [trick : magic-trick]) : Number
  (match trick
   [(Card-Trick decks vols) (* decks (expt 2 vols))]
   [(Guillotine r tig) (if tig 20 10)]
  ))


;Test Cases for trick-minutes funtion
(check-equal? (trick-minutes (Card-Trick 0 0)) 0)
(check-equal? (trick-minutes (Card-Trick 1 1)) 2)
(check-equal? (trick-minutes (Card-Trick 2 3)) 16)
(check-equal? (trick-minutes (Card-Trick 4 3)) 32)
(check-equal? (trick-minutes (Card-Trick 3 5)) 96)
(check-equal? (trick-minutes (Guillotine 1 false)) 10)
(check-equal? (trick-minutes (Guillotine 10 true)) 20)


;2.3 Low-degree Polynomials

;Polynomial Type Definition and Structures
(define-type Polynomial (U Linear Quadratic))

(struct Linear ([A : Real]
                 [B : Real])
  #:transparent)

(struct Quadratic ([A : Real]
                   [B : Real]
                   [C : Real])
  #:transparent)

;interp takes in a polynomial equation and a variable and computes the results of the equation
(define (interp [eqn : Polynomial][var : Real]) : Real
  (match eqn
    [(Linear a b) (+ (* a var) b)]
    [(Quadratic a b c) (+ (* a (expt var 2)) (* b var) c)]
  ))

;Test Cases for interp function
(check-equal? (interp (Linear 5 4) 2) 14)
(check-equal? (interp (Linear 6 13) 4) 37)
(check-equal? (interp (Linear 0 20) 10) 20)
(check-equal? (interp (Quadratic 2 4 1) 3) 31)
(check-equal? (interp (Quadratic 3 5 11) 4) 79)
(check-equal? (interp (Quadratic 1 0 0) 4) 16)

;2.4 Derivative

;derivative takes in a polynomial and returns its derivative
(define (derivative [eqn : Polynomial]) : Polynomial
  (match eqn
    [(Linear a b) (Linear 0 a)]
    [(Quadratic a b c) (Linear (* a 2) b)]
  ))

;Test Cases for Derivative function
(check-equal? (derivative (Quadratic 3 4 2)) (Linear 6 4))
(check-equal? (derivative (Quadratic 2 5 3)) (Linear 4 5))
(check-equal? (derivative (Quadratic 3 10 2)) (Linear 6 10))
(check-equal? (derivative (Linear 4 10)) (Linear 0 4))
(check-equal? (derivative (Linear 5 3)) (Linear 0 5))
(check-equal? (derivative (Linear 0 4)) (Linear 0 0))



;2.5 Binary Tree

;Data and Struct Definitions for Binary Tree
(define-type BTree (U Leaf Node))

(struct Leaf ([x : Symbol])
  #:transparent)

(struct Node ([right : BTree]
              [left : BTree])
  #:transparent)

;BTree Data Examples
(define leaf1
  (Leaf 'a))

(define leaf2
  (Leaf 'b))

(define leaf3
  (Leaf 'c))

(define leaf4
  (Leaf 'd))


(define node1
  (Node leaf1 leaf2))

(define node2
  (Node leaf3 leaf4))

(define node3
  (Node node1 node2))

(define node4
  (Node (Leaf 'er) node1))

(define node5
  (Node (Leaf 'tr) node2))

(define node6
  (Node node3 node1))

(define node7
  (Node node6 node5))

(define node8
  (Node node7 node4))

(define node9
  (Node node8 node3))

(define node10
  (Node node7 node9))

(define node11
  (Node (Leaf 'b)(Leaf'b)))




;2.6 Mirror

;mirror takes in binary tree, returns binary tree that is l/r mirror image of original
(define (mirror [t : BTree]): BTree
  (match t
    [(Node r l) (Node (mirror l) (mirror r))]
    [(Leaf s)(Leaf s)])
  )


;test cases for mirror 
(check-equal? (mirror node3) (Node (Node (Leaf 'd) (Leaf 'c)) (Node (Leaf 'b) (Leaf 'a))))
(check-equal? (mirror node1) (Node (Leaf 'b) (Leaf 'a)))
(check-equal? (mirror leaf3) (Leaf 'c))


;2.7 Min-Depth

;min-depth takes in a binary tree, returns the length of the shortest path to a leaf
(define (min-depth [t : BTree]): Integer
  (match t
    [(Node r l) (min (+ 1 (min-depth r)) (+ 1 (min-depth l)))]
    [(Leaf s) 0])
  )


;test cases for mirror
(check-equal? (min-depth node1) 1)
(check-equal? (min-depth node2) 1)
(check-equal? (min-depth node3) 2)
(check-equal? (min-depth node4) 1)
(check-equal? (min-depth node5) 1)
(check-equal? (min-depth node6) 2)
(check-equal? (min-depth node7) 2)
(check-equal? (min-depth node8) 2)
(check-equal? (min-depth node9) 3)
(check-equal? (min-depth node10) 3)
(check-equal? (min-depth leaf4) 0)


;2.8 Substitution

;subst takes in a source BTree, a symbol, and a replacement BT, and replaces every instance
;of that symbol with the replacement BT

(define (subst [src : BTree][smb : Symbol][rpl : BTree]): BTree
  (match src
    [(Node r l)(Node (subst r smb rpl)(subst l smb rpl))]
    [(Leaf s)
     (cond
       [(equal? smb s) rpl]
       [else (Leaf s)]
       )]
  ))

;test cases for subst
(check-equal? (subst node3 'a node2) (Node (Node (Node (Leaf 'c)(Leaf'd))(Leaf 'b))(Node (Leaf 'c) (Leaf 'd))))
(check-equal? (subst node1 'a node2) (Node (Node (Leaf 'c)(Leaf 'd)) (Leaf 'b)))
(check-equal? (subst node11 'b node1) (Node (Node (Leaf 'a)(Leaf 'b))(Node (Leaf 'a)(Leaf 'b))))
(check-equal? (subst leaf1 'a node2) (Node (Leaf 'c) (Leaf 'd)))


