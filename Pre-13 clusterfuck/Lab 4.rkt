;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; data def then function

; A PRS (PetRockStorage) is one of:
; - "King Paimon"
; - (make-bag String Number PRS)
(define-struct bag [color size contents])
; and represents the pet rock
; or a bag containing it (and possibly other bags) with a specific color and size
(define PRS-0 "King Paimon")  ; base case ;
(define PRS-1 (make-bag "blue" 37 PRS-0))
(define PRS-2 (make-bag "green" 50 PRS-1))
(define PRS-3 (make-bag "red" 20 PRS-1))

;; PRS -> ??
#;
(define (prs-temp prs)
  (cond
    [(string? prs) ...]
    [(bag? prs) ...
     (bag-color prs) ... (bag-size prs) ... (prs-temp (bag-contents prs)) ...]))

; Design a function which gives the size of the largest bag in a PRS
; largest-bag: PRS -> Number
; gives the size of the largest bag in a prs
(check-expect (largest-bag PRS-0) 0)
(check-expect (largest-bag PRS-1) 37)
(check-expect (largest-bag PRS-2) 50)
(check-expect (largest-bag PRS-3) 37)

(define (largest-bag prs)
  (cond
    [(string? prs) 0]
    [(bag? prs)
     (max (bag-size prs)
          (largest-bag (bag-contents prs)))]))

;ex 3

; color-check : PRS String -> Boolean
; determines if a bag of a specific color is within the given PRS
(check-expect (color-check PRS-0 "red") #false)
(check-expect (color-check PRS-1 "blue") #true)
(check-expect (color-check PRS-2 "blue") #true)
(check-expect (color-check PRS-3 "white") #false)

(define (color-check prs color)
  (cond
    [(string? prs) #false]
    [(bag? prs) (or
                 (string=? (bag-color prs) color)
                 (color-check (bag-contents prs) color))]))

;ex 4
; add-bag : PRS -> PRS
; adds 1 to each bag size in a PRS
(check-expect (add-bag PRS-0) PRS-0)
(check-expect (add-bag PRS-1) (make-bag "blue" 38 PRS-0))
(check-expect (add-bag PRS-2) (make-bag "green" 51 (make-bag "blue" 38 PRS-0)))

(define (add-bag prs)
  (cond
    [(string? prs) prs]
    [(bag? prs)
     (make-bag (bag-color prs) (add1 (bag-size prs)) (add-bag (bag-contents prs)))]))


;ex 5
; small-bag : PRS Number -> PRS
; discards all bags that are bigger than the specified size in a PRS
(check-expect (small-bag PRS-0 20) PRS-0)
(check-expect (small-bag PRS-2 40) (make-bag "blue" 37 PRS-0))
(check-expect (small-bag PRS-2 60) (make-bag "green" 50 PRS-1))
(check-expect (small-bag PRS-3 20) (make-bag "red" 20 PRS-0))

(define (small-bag prs num)
  (cond
    [(string? prs) PRS-0]
    [(bag? prs)
     (if
      (<= (bag-size prs) num)
      (make-bag (bag-color prs) (bag-size prs) (small-bag (bag-contents prs) num))
      (small-bag (bag-contents prs) num))]))


;ex 6
; well-stored? : PRS -> Boolean
; ensures if a PRS is comprised of successively smaller contents
(check-expect (well-stored? PRS-0) #true)
(check-expect (well-stored? PRS-1) #true)
(check-expect (well-stored? PRS-2) #true)
(check-expect (well-stored? PRS-3) #false)

(define (well-stored? prs)
  (cond
    [(string? prs) #true]
    [(bag? prs)
     (and
      (> (bag-size prs) (largest-bag (bag-contents prs)))
      (well-stored? (bag-contents prs)))]))

;ex 7
; A Nat is one of:
; - 0
; - (add1 Nat)
 
(define NAT-0 0)
(define NAT-1 (add1 NAT-0))
(define NAT-2 (add1 NAT-1))
(define NAT-3 (add1 NAT-2))
(define NAT-4 (add1 NAT-3))


#;
(define (nat-temp num)
  (cond
    [(zero? num) ...]
    [(positive? num) ... (nat-temp (sub1 num))]))
; try to get closer to base case
  
;ex 8
; double: Nat -> Nat
; doubles a natural number
(check-expect (double NAT-1) NAT-2)
(check-expect (double NAT-2) NAT-4)

(define (double nat)
  (cond
    [(zero? nat) NAT-0]
    [(positive? nat)
     (add1 (add1 (double (sub1 nat))))
        ]))


