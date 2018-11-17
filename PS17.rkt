;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname PS17) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Problem Set 17

; A Network is a [List-of Person]
 
; A Person is a (make-person String Belief [List-of String])
(define-struct person [name belief friends])
; and represents their name, their belief, and the name of their friends
; Examples:
(define PERSON-1 (make-person "Alice" "red" (list "Carol" "Heidi")))
(define PERSON-2 (make-person "Bob" "blue" (list "Carol" "Dan")))
(define PERSON-3 (make-person "Carol" "red" (list)))
(define PERSON-4 (make-person "Dan" "blue" (list "Carol" "Eric" "Frank" "Grace")))
(define PERSON-5 (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace")))
(define PERSON-6  (make-person "Frank" "blue" (list "Alice" "Bob" "Carol" "Dan" "Grace")))
; Template:
#;
(define (person-temp p)
  ... (person-name p) ... (belief-temp (person-belief p)) ... (person-friends p))
 
 
; A Belief is one of:
; - "blue"
; - "red"
;Template:
#;
(define (belief-temp b)
  (cond
    [(string=? b "blue") ...]
    [(string=? b "red") ...]))
                                                 
 
(define NETWORK
  (list
   (make-person "Alice" "red" (list "Carol" "Heidi"))
   (make-person "Bob" "blue" (list "Carol" "Dan"))
   (make-person "Carol" "red" (list))
   (make-person "Dan" "blue" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "blue" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "red" (list "Bob" "Frank"))
   (make-person "Heidi" "blue" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))

; Exercise 1

; update-network : Network -> Network
; updates every individual's belief to become the belief the majority of their friends have
; if there is a tie, then the belief is unchanged

; Exercise 2

; can-reach? : String String Network -> Boolean
; takes the names of two people and a network, asks if the first person can reach the second
;;;;;NOTE: Anyone can reach themselves, and anyone can reach another person through their friends if that friend has the same belief as them.