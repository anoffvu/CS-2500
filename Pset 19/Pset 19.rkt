;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Pset 19|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;ex 1

; relative->absolute : [List-of Number] -> [List-of Number]
; converts relative distances between points to absolute values

(check-expect (relative->absolute (list)) (list))
(check-expect (relative->absolute (list 0 0 0)) (list 0 0 0))
(check-expect (relative->absolute (list 20)) (list 20))
(check-expect (relative->absolute (list 0 10 30 0 40 10)) (list 0 10 40 40 80 90))

(define (relative->absolute lon)
  (local [(define (relative->absolute/a lon sum-so-far)
            (cond [(empty? lon) '()]
                  [(cons? lon) (cons (+ (first lon) sum-so-far)
                                     (relative->absolute/a (rest lon) (+ (first lon) sum-so-far)))]))]
    (relative->absolute/a lon 0)))


;ex 2

; stops when someone buys with 10 but no 5 dollar bills left
; credit card and exact $5 always goes thru

; A Payment is one of:
; - 5
; - 10
; - "CC"
; and represents  payment in $5, $10, or credit card

(define PAY-5 5)
(define PAY-10 10)
(define PAY-CC "CC")

#;
(define (payment-temp p)
  (cond [(= 5 p) ...]
        [(= 10 p) ...]
        [(string? p) ...]))


; (define LOP-1 (list 5 "CC" "CC" 5 5 5 10 5))
(define LOP-1 (list PAY-5 PAY-CC PAY-CC PAY-5 PAY-5 PAY-5 PAY-10 PAY-5))
; (define LOP-2 (list 5 "CC" "CC" 5 5 5 10 10 5 10 5))
(define LOP-2 (list PAY-5 PAY-CC PAY-CC PAY-5 PAY-5 PAY-5 PAY-10 PAY-10 PAY-5 PAY-10 PAY-5))


; ticket-calc : NonNegativeInteger [List-of Payment] -> Number
; calculates the number of tickets able to be sold

(check-expect (ticket-calc 0 (list 10)) 0)
(check-expect (ticket-calc 1 (list 10)) 1)
(check-expect (ticket-calc 2 (list 10)) 1)
(check-expect (ticket-calc 1 (list 10 5 "CC" 5)) 4)
(check-expect (ticket-calc 1 (list)) 0)
(check-expect (ticket-calc 0 LOP-1) 6)
(check-expect (ticket-calc 2 LOP-2) 9)

(define (ticket-calc n lop)
  (local [; ticket-calc/a : Number [List-of Payment] Number -> NUmber
          ; calculates the number of tickets able to be sold
          ; Accumulator: keeps track of the number of tickets sold so far
          (define (ticket-calc/a n lop tickets-sold)
            (cond [(empty? lop) tickets-sold]
                  [(cons? lop) (if (or (string? (first lop)) (= (first lop) 5))
                                   (ticket-calc/a n (rest lop) (add1 tickets-sold))
                                   (if (= n 0)
                                       tickets-sold
                                       (ticket-calc/a (sub1 n) (rest lop) (add1 tickets-sold))))]))]
    (ticket-calc/a n lop 0)))


;ex 3

; A Network is a [List-of Person]
 
; A Person is a (make-person String Belief [List-of String])
(define-struct person [name belief friends])
; and represents their name, their belief, and the name of their friends
; Examples:
(define ALICE (make-person "Alice" "red" (list "Carol" "Heidi")))
(define BOB (make-person "Bob" "blue" (list "Carol" "Dan")))
(define CAROL (make-person "Carol" "red" (list)))
(define DAN (make-person "Dan" "blue" (list "Carol" "Eric" "Frank" "Grace")))
(define ERIC (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace")))
(define FRANK (make-person "Frank" "blue" (list "Alice" "Bob" "Carol" "Dan" "Grace")))
(define GRACE (make-person "Grace" "red" (list "Bob" "Frank")))
(define HEIDI (make-person "Heidi" "blue" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace")))
; Template:
#;
(define (person-temp p)
  ... (person-name p) ... (belief-temp (person-belief p)) ... (person-friends p))
 
; A Belief is one of:
; - "blue"
; - "red"
; Template:
#;
(define (belief-temp b)
  (cond
    [(string=? b "blue") ...]
    [(string=? b "red") ...]))
 
(define NETWORK-1
  (list
   (make-person "Alice" "red" (list "Carol" "Heidi"))
   (make-person "Bob" "blue" (list "Carol" "Dan"))
   (make-person "Carol" "red" (list))
   (make-person "Dan" "blue" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "blue" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "red" (list "Bob" "Frank"))
   (make-person "Heidi" "blue" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))

(define NETWORK-2
  (list
   (make-person "Alice" "red" (list "Carol" "Heidi"))
   (make-person "Bob" "blue" (list "Carol" "Dan"))
   (make-person "Carol" "red" (list))
   (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "red" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "blue" (list "Bob" "Frank"))
   (make-person "Heidi" "red" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))

(define NETWORK-3
  (list
   (make-person "Alice" "red" (list "Carol" "Heidi"))
   (make-person "Bob" "red" (list "Carol" "Dan"))
   (make-person "Carol" "red" (list))
   (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "red" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "red" (list "Bob" "Frank"))
   (make-person "Heidi" "red" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))

(define NETWORK-4
  (list
   (make-person "Alice" "blue" (list "Carol" "Heidi"))
   (make-person "Bob" "blue" (list "Carol" "Dan"))
   (make-person "Carol" "blue" (list))
   (make-person "Dan" "blue" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "red" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "red" (list "Bob" "Frank"))
   (make-person "Heidi" "red" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))

(define NETWORK-5
  (list
   (make-person "Alice" "blue" (list "Carol" "Heidi"))
   (make-person "Bob" "blue" (list "Carol" "Dan"))
   (make-person "Carol" "blue" (list))
   (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "blue" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "blue" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "red" (list "Bob" "Frank"))
   (make-person "Heidi" "blue" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))

(define NETWORK-10
  (list
   (make-person "Alice" "red" (list "Bob" "Carol"))
   (make-person "Bob" "red" (list))
   (make-person "Carol" "red" (list "Dan"))
   (make-person "Dan" "red" (list "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Heidi"))
   (make-person "Frank" "red" (list "Heidi" "Isidore"))
   (make-person "Grace" "red" (list "Isidore"))
   (make-person "Heidi" "red" (list))
   (make-person "Isidore" "red" (list))))

(define NETWORK-10b
  (list
   (make-person "Alice" "red" (list "Bob" "Carol"))
   (make-person "Bob" "red" (list))
   (make-person "Carol" "red" (list "Dan"))
   (make-person "Dan" "blue" (list "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Heidi"))
   (make-person "Frank" "red" (list "Heidi" "Isidore"))
   (make-person "Grace" "red" (list "Isidore"))
   (make-person "Heidi" "red" (list))
   (make-person "Isidore" "red" (list))))

(define NETWORK-11
  (list
   (make-person "Alice" "red" (list "Bob" "Carol"))
   (make-person "Bob" "blue" (list "Isidore"))
   (make-person "Carol" "red" (list))
   (make-person "Isidore" "blue" (list))))

(define NETWORK-12
  (list
   (make-person "Alice" "red" (list "Bob" "Carol"))
   (make-person "Bob" "red" (list "Isidore"))
   (make-person "Carol" "red" (list))
   (make-person "Isidore" "blue" (list))))


; can-reach? : Network String String -> Boolean
; determines if the first person can reach the second person
(check-expect (can-reach? NETWORK-10 "Bob" "Carol") #false)
(check-expect (can-reach? NETWORK-10 "Alice" "Isidore") #true)
(check-expect (can-reach? NETWORK-10b "Alice" "Isidore") #false)
(check-expect (can-reach? NETWORK-11 "Alice" "Isidore") #false)
(check-expect (can-reach? NETWORK-12 "Alice" "Isidore") #true)
(check-expect (can-reach? NETWORK-12 "Alice" "Alice") #true)

(define (can-reach? nw p1 p2)
  (local [; grabs the friends of the first person
          (define BEGINNING-FRIENDS (get-friends nw p1))
          ; checks if the end person is within the starting friends
          (define IS-END-IN-BEGINNING-FRIENDS (member? p2 BEGINNING-FRIENDS))
          ; filters out the friends only for ones with the same belief
          (define SAME-BELIEF-BEGINNING-FRIENDS
            (local [(define (same-belief? friend)
                      (string=? (belief-grabber nw p1) (belief-grabber nw friend)))]
              (filter same-belief? BEGINNING-FRIENDS)))]
    (or (or IS-END-IN-BEGINNING-FRIENDS (string=? p1 p2))
        (connected-thru-friends? nw SAME-BELIEF-BEGINNING-FRIENDS p2))))
; belief-grabber : Network String -> Belief
; gets the belief of a person given their name and network
(check-expect (belief-grabber NETWORK-10 "Alice") "red")
(check-expect (belief-grabber NETWORK-10b "Dan") "blue")
(check-expect (belief-grabber NETWORK-10b "Bob") "red")
(check-expect (belief-grabber NETWORK-11 "Isidore") "blue")
(check-expect (belief-grabber NETWORK-12 "Carol") "red")

(define (belief-grabber nw p)
  (cond
    [(empty? nw) '()]
    [(cons? nw) (if (string=? p (person-name (first nw)))
                    (person-belief (first nw))
                    (belief-grabber (rest nw) p))]))

; get-friends : Network String -> [List-of String]
; gets the list of friends of a person given their name and network
(check-expect (get-friends NETWORK-10 "Alice") (list "Bob" "Carol"))
(check-expect (get-friends NETWORK-10b "Bob") (list ))
(check-expect (get-friends NETWORK-10b "Dan") (list "Eric" "Frank" "Grace"))
(check-expect (get-friends NETWORK-11 "Alice") (list "Bob" "Carol"))
(check-expect (get-friends NETWORK-12 "Isidore") (list ))
(define (get-friends nw p)
  (cond
    [(empty? nw) '()]
    [(cons? nw) (if (string=? p (person-name (first nw)))
                    (person-friends (first nw))
                    (get-friends (rest nw) p))]))


; connected-thru-friends? : Network [List-of String] String -> Boolean
; checks to see if there is a connection between the friends and the end person (p2)

(check-expect (connected-thru-friends? NETWORK-10 (list "Bob" "Carol") "Isidore") #true)
(check-expect (connected-thru-friends? NETWORK-10b (list "Bob" "Carol") "Isidore") #false)
(check-expect (connected-thru-friends? NETWORK-11 (list "Bob" "Carol") "Isidore") #true)
(check-expect (connected-thru-friends? NETWORK-12 (list "Bob" "Carol") "Isidore") #true)
(check-expect (connected-thru-friends? NETWORK-11 (list) "Isidore") #false)
(check-expect (connected-thru-friends? NETWORK-10 (list) "Isidore") #false)


(define (connected-thru-friends? nw friends p2)
  (local [(define (can-reach-wrapper start-friend)
            (can-reach? nw start-friend p2))]
    (ormap can-reach-wrapper friends)))

            