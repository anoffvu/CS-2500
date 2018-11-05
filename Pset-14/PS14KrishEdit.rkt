;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname PS14KrishEdit) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Problem Set 14

(define-struct leaf [])
(define-struct node [data left right])
; A [Tree X] is one of:
; - (make-leaf)
; - (make-node X [Tree X] [Tree X])

; Examples:
(define TREE-0 (make-leaf))
(define TREE-1 (make-node 4 TREE-0 TREE-0))
(define TREE-2 (make-node 1 TREE-0 TREE-0))
(define TREE-3 (make-node 5 TREE-1 TREE-2))
(define TREE-4 (make-node 9 TREE-3 TREE-0))

; Template:
#;
(define (tree-temp t)
  (cond
    [(leaf? t) ...]
    [(node? t) ... (node-data t) ...
               ... (tree-temp (node-left t)) ...
               ... (tree-temp (node-right t)) ...]))
 
 
(define-struct family-member [name yob])
; A FamilyMember is a (make-family-member String Number)
; - where name is the family member's name
; - and yob is their year of birth
; Examples:
(define FAMILY-MEMBER-1 (make-family-member "james" 1999))
(define FAMILY-MEMBER-2 (make-family-member "krish" 2000))
(define FAMILY-MEMBER-3 (make-family-member "alan" 2001))
(define FAMILY-MEMBER-4 (make-family-member "alan" 1995))
(define FAMILY-MEMBER-5 (make-family-member "james" 1990))
; Template:
#;
(define (family-member-temp fm)
  (... (family-member-name fm) ... (family-member-yob fm) ...))


; Exercise 1

; An FT is a [Tree FamilyMember] and represents a family tree, with the youngest
; family member at the root.

; Examples:
(define FT-0 (make-leaf))
(define FT-1 (make-node FAMILY-MEMBER-1 FT-0 FT-0))
(define FT-2 (make-node FAMILY-MEMBER-2 FT-0 FT-0))
(define FT-3 (make-node FAMILY-MEMBER-3 FT-1 FT-2))
(define FT-4 (make-node FAMILY-MEMBER-4 FT-3 FT-0))
(define FT-5 (make-node FAMILY-MEMBER-5 FT-0 FT-3))

; Template:
#;
(define (ft-temp ft)
  (cond
    [(leaf? ft) ...]
    [(node? ft) ... (family-member-temp (node-data ft)) ...
                ... (ft-temp (node-left ft)) ...
                ... (ft-temp (node-right ft)) ...]))

; Exercise 2

(define CURRENT-YEAR 2018)

; list-of-ages : FT -> [List-of Nat]
; returns the list of everyone's age (in years) in a FT, assuming everyone
; was born at midnight on Jan 1st and is still alive
(check-expect (list-of-ages FT-0) '())
(check-expect (list-of-ages FT-1) (list 19))
(check-expect (list-of-ages FT-2) (list 18))
(check-expect (list-of-ages FT-3) (list 17 19 18))
(check-expect (list-of-ages FT-4) (list 23 17 19 18))
(check-expect (list-of-ages FT-5) (list 28 17 19 18))

(define (list-of-ages ft)
  (cond
    [(leaf? ft) '()]
    [(node? ft) (append (list (calc-age (node-data ft)))
                        (list-of-ages (node-left ft))
                        (list-of-ages (node-right ft)))]))

; calc-age : FamilyMember -> Nat
; calculates the age of a person by subtracting the current year by year of birth given
(check-expect (calc-age FAMILY-MEMBER-1) 19)
(check-expect (calc-age FAMILY-MEMBER-2) 18)
(check-expect (calc-age FAMILY-MEMBER-3) 17)
(check-expect (calc-age FAMILY-MEMBER-4) 23)
(check-expect (calc-age FAMILY-MEMBER-5) 28)

(define (calc-age fm)
  (- CURRENT-YEAR (family-member-yob fm)))

; Exercise 3

; max-age : FT -> Nat
; produces the age of the oldest family member in a given FT
(check-expect (max-age FT-0) 0)
(check-expect (max-age FT-1) 19)
(check-expect (max-age FT-2) 18)
(check-expect (max-age FT-3) 19)
(check-expect (max-age FT-4) 23)
(check-expect (max-age FT-5) 28)
(define (max-age ft)
  (cond
    [(leaf? ft) 0]
    [(node? ft) (max
                 (calc-age (node-data ft)) 
                 (max-age (node-left ft))
                 (max-age (node-right ft)))]))

; Exercise 4

; num-generations : FT -> Nat
; produces the number of generations shown by a given FT
(check-expect (num-generations FT-0) 0)
(check-expect (num-generations FT-1) 1)
(check-expect (num-generations FT-2) 1)
(check-expect (num-generations FT-3) 2)
(check-expect (num-generations FT-4) 3)
(check-expect (num-generations FT-5) 3)
(define (num-generations ft)
  (cond
    [(leaf? ft) 0]
    [(node? ft) (add1 (max (num-generations (node-left ft))
                           (num-generations (node-right ft))))]))

; Exercise 5


; any-same-name? : FT -> Boolean
; determines if anyone in a FT has been named after their ancestor
(check-expect (any-same-name? FT-0) #false)
(check-expect (any-same-name? FT-1) #false)
(check-expect (any-same-name? FT-2) #false)
(check-expect (any-same-name? FT-3) #false)
(check-expect (any-same-name? FT-4) #true)
(check-expect (any-same-name? FT-5) #true)
(define (any-same-name? ft)
  (cond [(leaf? ft) #false]
        [(node? ft) (or
                     (same-name-in-this-tree? ft)
                     (any-same-name? (node-left ft))
                     (any-same-name? (node-right ft)))]))



; same-name-in-this-tree? : FT -> Boolean
; determines whether the child of an FT was named after any ancestors
(check-expect (same-name-in-this-tree? FT-0) #false)
(check-expect (same-name-in-this-tree? FT-1) #false)
(check-expect (same-name-in-this-tree? FT-3) #false)
(check-expect (same-name-in-this-tree? FT-4) #true)
(check-expect (same-name-in-this-tree? FT-5) #true)

(define (same-name-in-this-tree? ft)
  (cond
    [(leaf? ft) #false]
    [(node? ft) (or (member? (family-member-name (node-data ft))
                             (list-of-names (node-left ft)))
                    (member? (family-member-name (node-data ft))
                             (list-of-names (node-right ft))))]))

; list-of-names : FT -> [List-of String]
; makes a FT into a list of all names in the tree
(check-expect (list-of-names FT-0) (list ))
(check-expect (list-of-names FT-1) (list "james"))
(check-expect (list-of-names FT-2) (list "krish"))
(check-expect (list-of-names FT-3) (list "alan" "james" "krish"))
(check-expect (list-of-names FT-4) (list "alan" "alan" "james" "krish"))
(check-expect (list-of-names FT-5) (list "james" "alan" "james" "krish"))
(define (list-of-names ft)
  (cond
    [(leaf? ft) '()]
    [(node? ft) (append (list (family-member-name (node-data ft)))
                        (list-of-names (node-left ft))
                        (list-of-names (node-right ft)))]))


     
         

              
                                        
                                   
                     

