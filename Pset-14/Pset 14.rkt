;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Pset 14|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct leaf [])
(define-struct node [data left right])
; A [Tree X] is one of:
; - (make-leaf)
; - (make-node X [Tree X] [Tree X])

(define TREE-0 (make-leaf))
(define TREE-1 (make-node "Person 1" TREE-0 TREE-0))
(define TREE-2 (make-node "Person 2" TREE-0 TREE-1))

#;
(define (tree-temp t)
  (cond [(leaf? t) ...]
        [(node? t) ... (node-data t) ...
                   ... (node-left t) ...
                   ... (node-right t) ...]))
 

(define CURRENT-YEAR 2018)

(define-struct family-member [name yob])
; A FamilyMember is a (make-family-member String Number)
; - where name is the family member's name
; - and yob is their year of birth

(define FAMILYMEMBER-1 (make-family-member "Person 1" 2001))
(define FAMILYMEMBER-2 (make-family-member "Person 2" 2002))
(define FAMILYMEMBER-3 (make-family-member "Person duplicate" 2003))
(define FAMILYMEMBER-4 (make-family-member "Person duplicate" 2004))
(define FAMILYMEMBER-5 (make-family-member "Person 5" CURRENT-YEAR))
(define FAMILYMEMBER-6 (make-family-member "Person 6" 1900))

#;
(define (family-member-temp fm)
  ... (family-member-name fm) ... (family-member-yob fm) ...)
 
; An FT is a [Tree FamilyMember] and represents a family tree, with the youngest
; family member at the root.

(define FT-1 (make-leaf))
(define FT-2 (make-node FAMILYMEMBER-1 (make-leaf) (make-leaf)))
(define FT-3 (make-node FAMILYMEMBER-2 (make-node FAMILYMEMBER-3 (make-leaf) (make-leaf))
                        (make-node FAMILYMEMBER-4 (make-leaf) (make-leaf))))
(define FT-4 (make-node FAMILYMEMBER-5 FT-1 FT-3))
(define FT-5 (make-node FAMILYMEMBER-1 (make-node (make-family-member "Person 1" 1999)
                                                  (make-leaf) (make-leaf))
                        (make-node FAMILYMEMBER-2 (make-leaf) (make-leaf))))
(define FT-6 (make-node FAMILYMEMBER-1 (make-node FAMILYMEMBER-3
                                                  (make-node
                                                   (make-family-member "Person 1" 1999)
                                                   (make-leaf) (make-leaf))
                                                  (make-leaf))
                        (make-node FAMILYMEMBER-2 (make-leaf) (make-leaf))))
(define FT-7 (make-node FAMILYMEMBER-1 (make-node
                                        (make-family-member "George" 2000)
                                        (make-node
                                         (make-family-member "George" 1999)
                                         (make-leaf) (make-leaf))
                                        (make-leaf))
                        (make-node FAMILYMEMBER-2 (make-leaf) (make-leaf))))

#;
(define (ft-temp ft)
  (cond [(leaf? ft) ... ]
        [(node? ft) ... (node-data ft) ...
                    ... (family-member-temp (node-left ft)) ...
                    ... (family-member-temp (node-right ft)) ...]))


;ex 2

; calc-age : FamilyMember -> Nat
; Calculates the age of a family member in years
(check-expect (calc-age FAMILYMEMBER-1) 17)
(check-expect (calc-age FAMILYMEMBER-3) 15)
(check-expect (calc-age FAMILYMEMBER-5) 0)
(check-expect (calc-age FAMILYMEMBER-6) 118)

(define (calc-age fm)
  (- CURRENT-YEAR (family-member-yob fm)))


; ft-all-ages : FT -> [List-of Nat]
; creates a list of all the ages in a given family tree
(check-expect (ft-all-ages FT-1) '())
(check-expect (ft-all-ages FT-2) (list 17))
(check-expect (ft-all-ages FT-3) (list 16 15 14 ))

(define (ft-all-ages ft)
  (cond [(leaf? ft) '()]
        [(node? ft) (append (list (calc-age (node-data ft))) (ft-all-ages (node-left ft))
                            (ft-all-ages (node-right ft)))]))


;ex 3

; ft-max-age : FT -> Num
; finds the oldest person in a family tree
(check-expect (ft-max-age FT-1) 0)
(check-expect (ft-max-age FT-2) 17)
(check-expect (ft-max-age FT-3) 16)
(check-expect (ft-max-age FT-4) 16)

(define (ft-max-age ft)
  (cond [(leaf? ft) 0]
        [(node? ft) (foldr max 0 (ft-all-ages ft))]))


;ex 4

; total-generations : FT -> Number
; calculates the number of generations in an FT
(check-expect (total-generations FT-1) 0)
(check-expect (total-generations FT-2) 1)
(check-expect (total-generations FT-3) 2)
(check-expect (total-generations FT-4) 3)

(define (total-generations ft)
  (cond [(leaf? ft) 0 ]
        [(node? ft) (add1 (max (total-generations (node-left ft))
                               (total-generations (node-right ft))))]))


;ex 5

; duplicate-name-in-all-trees? : FT -> Boolean
; determines if anyone in an FT has been named after their ancestor
(check-expect (duplicate-name-in-all-trees? FT-6) #true)
(check-expect (duplicate-name-in-all-trees? FT-5) #true)
(check-expect (duplicate-name-in-all-trees? FT-4) #false)
(check-expect (duplicate-name-in-all-trees? FT-2) #false)
(check-expect (duplicate-name-in-all-trees? FT-1) #false)

(define (duplicate-name-in-all-trees? ft)
  (cond [(leaf? ft) #false]
        [(node? ft) (or
                     (duplicate-name-in-this-tree? ft)
                     (duplicate-name-in-all-trees? (node-left ft))
                     (duplicate-name-in-all-trees? (node-right ft)))]))


; duplicate-name-in-this-tree? : String FT -> Boolean
; checks if the a person was named after any of his ancestors
(check-expect (duplicate-name-in-this-tree? FT-1) #false)
(check-expect (duplicate-name-in-this-tree? FT-2) #false)
(check-expect (duplicate-name-in-this-tree? FT-3) #false)
(check-expect (duplicate-name-in-this-tree? FT-5) #true)
(check-expect (duplicate-name-in-this-tree? FT-6) #true)
(check-expect (duplicate-name-in-this-tree? FT-7) #false)

(define (duplicate-name-in-this-tree? ft)
  (cond [(leaf? ft) #false]
        [(node? ft) (or
                    (member? (family-member-name (node-data ft)) (ft-all-names (node-left ft)))
                    (member? (family-member-name (node-data ft)) (ft-all-names (node-right ft))))]))
 

; ft-all-names : FT -> [List-of String]
; creates a list of all the names in a given family tree
(check-expect (ft-all-names FT-1) '())
(check-expect (ft-all-names FT-2) (list "Person 1"))
(check-expect (ft-all-names FT-3) (list "Person 2" "Person duplicate" "Person duplicate"))
(check-expect (ft-all-names FT-4) (list "Person 5" "Person 2" "Person duplicate" "Person duplicate"))

(define (ft-all-names ft)
  (cond [(leaf? ft) '()]
        [(node? ft) (append (list (family-member-name (node-data ft))) (ft-all-names (node-left ft))
                            (ft-all-names (node-right ft)))]))