;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Pset 17|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Network is a [List-of Person]
 
; A Person is a (make-person String Belief [List-of String])
(define-struct person [name belief friends])
; and represents their name, their belief, and the name of their friends
 
; A Belief is one of:
; - "blue"
; - "red"
 
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

; update-network : Network -> Network
; updates a given each person in a network

(check-expect (update-network NETWORK-1) NETWORK-2)

; I assumed that there can only be unique names, if there two different people were named
; the same thing, the definition of a person wouldn't work the way it was intended to.
; This way, we can store a list of all reds and check if a person is a "red" by member?

; I also tried to keep certain modularity within the program so that it would require less
; changes

(define (update-network network)
  (local [(define INIT-NETWORK network)
          
          (define (grab-red-names network)
                                   (local [(define (red? person)
                                             (string=? (person-belief person) "red"))]
                                     (map person-name (filter red? network))))
          
          (define INIT-RED-NAMES (grab-red-names network))

          (define (grab-blue-names network)
                                   (local [(define (blue? person)
                                             (string=? (person-belief person) "blue"))]
                                     (map person-name (filter blue? network))))
       
          (define INIT-BLUE-NAMES (grab-blue-names network))
          

            (define (party-friends p party)
              (local [(define (member-of-blues? name)
                        (member? name INIT-BLUE-NAMES))
                      (define (member-of-reds? name)
                        (member? name INIT-RED-NAMES))]
                (if (string=? party "red")
                    (filter member-of-reds? (person-friends p))
                    (filter member-of-blues? (person-friends p)))))

            (define (person-update p)
              (cond
                [(> (length (party-friends p "red")) (length (party-friends p "blue")))
                 (make-person (person-name p) "red" (person-friends p))]
                [(> (length (party-friends p "blue")) (length (party-friends p "red")))
                 (make-person (person-name p) "blue" (person-friends p))]
                [else p]))]
(map person-update network)))


;ex 2

(define NETWORK-10
  (list
   (make-person "Alice" "red" (list "Carol" "Heidi"))
   (make-person "Bob" "red" (list "Carol" "Dan"))
   (make-person "Carol" "red" (list))
   (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "red" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "red" (list "Bob" "Frank"))
   (make-person "Heidi" "red" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))

(define NETWORK-11
  (list
   (make-person "Alice" "red" (list "Carol" "Heidi"))
   (make-person "Bob" "blue" (list "Carol" "Dan"))
   (make-person "Carol" "red" (list))
   (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "red" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "blue" (list "Bob" "Frank"))
   (make-person "Heidi" "red" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))
; can-reach? : Network String String -> Boolean
; determines if the first person can reach the second person

(define (can-reach? nw p1 p2)
  (local [(define BEGINNING-FRIENDS (get-friends nw p1))
          (define IS-END-IN-BEGINNING-FRIENDS (member? p2 BEGINNING-FRIENDS))
          (define SAME-BELIEF-BEGINNING-FRIENDS
            (local [(define (same-belief? p1 friend)
                      (string=? (party-grabber nw p1) (party-grabber nw friend)))]
            (filter same-belief? BEGINNING-FRIENDS)))]
  (if IS-END-IN-BEGINNING-FRIENDS
      #true
      (connected-thru-friends? nw BEGINNING-FRIENDS p2))))

(define (party-grabber nw p)
  (cond
    [(empty? nw) '()]
    [(cons? nw) (if (string=? p (person-name (first nw)))
                    (person-belief (first nw))
                    (get-friends (rest nw) p))]))

(define (get-friends nw p)
  (cond
    [(empty? nw) '()]
    [(cons? nw) (if (string=? p (person-name (first nw)))
                    (person-friends (first nw))
                    (get-friends (rest nw) p))]))

(define (connected-thru-friends? nw friends p2)
  (local [(define (can-reach-modified start)
            (can-reach? nw start p2))]
  (ormap can-reach-modified friends)))
  
  

      