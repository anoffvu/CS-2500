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
  (list ALICE BOB CAROL DAN ERIC FRANK GRACE HEIDI))

(define NETWORK-2
  (list
   (make-person "Alice" "red" (list "Carol" "Heidi"))
   (make-person "Bob" "blue" (list "Carol" "Dan"))
   (make-person "Carol" "red" (list))
   (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Carol" "Dan" "Frank" "Grace" "Bob"))
   (make-person "Frank" "red" (list "Alice" "Carol" "Dan" "Grace" "Bob"))
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

(define NETWORK-CYCLES-1
  (list
   (make-person "Alice" "red" (list "Bob"))
   (make-person "Bob" "red" (list "Alice" "Chris"))
   (make-person "Chris" "blue" (list))))

(define NETWORK-CYCLES-2
  (list
   (make-person "Alice" "red" (list "Bob"))
   (make-person "Bob" "red" (list "Alice" "Dan"))
   (make-person "Chris" "blue" (list))
   (make-person "Dan" "blue" (list))))

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
; Check Expects:
; these check basic functionality on an acyclic network
(check-expect (can-reach? NETWORK-10 "Bob" "Carol") #false)
(check-expect (can-reach? NETWORK-10 "Alice" "Isidore") #true)
(check-expect (can-reach? NETWORK-10b "Alice" "Isidore") #false)
(check-expect (can-reach? NETWORK-11 "Alice" "Isidore") #false)
(check-expect (can-reach? NETWORK-12 "Alice" "Isidore") #true)
(check-expect (can-reach? NETWORK-12 "Alice" "Alice") #true)
; these check on a cyclic network
(check-expect (can-reach? NETWORK-1 "Alice" "Eric") #false)
(check-expect (can-reach? NETWORK-1 "Carol" "Bob") #false)
(check-expect (can-reach? NETWORK-1 "Dan" "Alice") #true)
(check-expect (can-reach? NETWORK-2 "Dan" "Bob") #true)
(check-expect (can-reach? NETWORK-3 "Alice" "Dan") #true)
(check-expect (can-reach? NETWORK-CYCLES-1 "Alice" "Chris") #true)
(check-expect (can-reach? NETWORK-CYCLES-2 "Alice" "Chris") #false)

(define (can-reach? nw p1 p2)
  (local [; can-reach?/a Network String String [List-of String] -> Boolean
          ; determines if one person can reach another person in a network
          ; Accumulator: every iteration keeps a history of nodes visited
          (define (can-reach?/a nw p1 p2 visited)
            (local [; grabs the friends of the first person
                    (define BEGINNING-FRIENDS (get-friends nw p1))
                    ; checks if the end person is within the starting friends
                    (define IS-END-IN-BEGINNING-FRIENDS (member? p2 BEGINNING-FRIENDS))
                    ; filters out the friends only for ones with the same belief
                    (define SAME-BELIEF-FRIENDS
                      (local [; same-belief? : String -> Boolean
                              ; tests to see if the two friends have the same belief
                              ; given "Alice" and "Alice", returns #true
                              ; given "Alice" and "Bob" of NETWORK-12, returns #true
                              ; given "Alice" and "Isidore" of NETWORK-12, returns #false
                              (define (same-belief? friend)
                                (string=? (belief-grabber nw p1) (belief-grabber nw friend)))]
                        (filter same-belief? BEGINNING-FRIENDS)))
                    ; not-visited? : String -> Boolean
                    ; determines if a person has been visited already
                    ; given "Alice" with a visited list of (list "Alice" "Bob"), returns #false
                    ; given "Steve" with a visited list of (list "Alice" "Bob"), returns #true
                    (define (not-visited? p)
                      (not (member? p visited)))
                    ; trims the list down to only the friends that we have not visited before
                    (define UNVISITED-SAME-BELIEF-FRIENDS
                      (filter not-visited? SAME-BELIEF-FRIENDS))
                    ; connected-thru-friends? : Network [List-of String] String -> Boolean
                    ; checks to see if there is a connection between the friends and the end person
                    ; assume for these check-expects that it has visited no one:
                    ; given NETWORK-1 (list "Carol" "Heidi") "Bob", returns #true
                    ; given NETWORK-1 (list "Carol" "Heidi") "Frank", returns #false
                    ; given NETWORK-11 (list "Alice") "Isidore", returns #false
                    ; Assume for these check exepct that it has visited Heidi:
                    ; given NETWORK-1 (list "Carol" "Heidi") "Bob", returns #false
                    ; given NETWORK-1 (list "Carol" "Heidi") "Grace", returns #false
                    (define (connected-thru-friends? nw friends p2 visited)
                      (local [; can-reach-wrapper: String -> Boolean
                              ; tests to see if it can reach a person
                              ; assume for these check-expects that it has visited no one and
                              ; we are searching using NETWORK-1:
                              ; given "Heidi" while p2 is "Bob", returns #true
                              ; given "Carol" while p2 is "Bob", returns #false
                              ; given "Dan" while p2 is "Alice", returns #true
                              ; Assume for these check exepct that it has visited Heidi:
                              ; given "Alice" while p2 is "Bob", returns #false
                              ; given "Alice" while p2 is "Carol", returns #true
                              (define (can-reach-wrapper start-friend)
                                (can-reach?/a nw start-friend p2 visited))]
                        (ormap can-reach-wrapper friends)))]
              
              (or IS-END-IN-BEGINNING-FRIENDS 
                  (connected-thru-friends? nw UNVISITED-SAME-BELIEF-FRIENDS p2 (append visited
                                                                                       (list p1))))))]
    (or (string=? p1 p2)
        (can-reach?/a nw p1 p2 '()))))


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

