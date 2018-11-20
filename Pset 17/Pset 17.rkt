;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Pset 17|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; update-network : Network -> Network
; updates a given each person in a network

(check-expect (update-network NETWORK-1) NETWORK-2)
; maybe more CEs?


(define (update-network network)
  (local [(define INIT-NETWORK network)
          ; grab-red-names : Network -> [List-of String]
          ; grabs all the red names from a network
          ; Given NETWORK-1, return (list "Alice" "Carol" "Eric" "Grace")
          ; Given NETWORK-2, return (list "Alice" "Carol" "Dan" "Eric" "Frank" "Heidi")
          (define (grab-red-names network)
            (local [(define (red? person)
                      (string=? (person-belief person) "red"))]
              (map person-name (filter red? network))))
          ; puts all the people's names with a red belief into a constant
          (define INIT-RED-NAMES (grab-red-names network))
          ; grab-blue-names : Network -> [List-of String]
          ; grabs all the blue names from a network
          ; Given NETWORK-1, return (list "Bob" "Dan" "Frank" "Heidi")
          ; Given NETWORK-2, return (list "Bob" "Grace")
          (define (grab-blue-names network)
            (local [(define (blue? person)
                      (string=? (person-belief person) "blue"))]
              (map person-name (filter blue? network))))
          ; stores all the people's names with a blue belief into a constant
          (define INIT-BLUE-NAMES (grab-blue-names network))
          
          ; belief-of-friends : Person Belief -> [List-of String]
          ; creates a list of all the friends with the given belief
          ; Given GRACE and "red", return '()
          ; Given DAN and "red", return (list "Carol" "Eric" "Grace")
          ; Given BOB and "blue", return (list "Dan")
          (define (belief-of-friends p belief)
            (local [(define (member-of-blues? name)
                      (member? name INIT-BLUE-NAMES))
                    (define (member-of-reds? name)
                      (member? name INIT-RED-NAMES))]
              (if (string=? belief "red")
                  (filter member-of-reds? (person-friends p))
                  (filter member-of-blues? (person-friends p)))))
          ; person-update : Person -> Person
          ; updates a person's belief based on his or her friends' beliefs
          ; Given ALICE, return ALICE
          ; Given DAN, return (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
          ; Given GRACE, return (make-person "Grace" "red" (list "Bob" "Frank"))
          (define (person-update p)
            (cond
              [(> (length (belief-of-friends p "red")) (length (belief-of-friends p "blue")))
               (make-person (person-name p) "red" (person-friends p))]
              [(> (length (belief-of-friends p "blue")) (length (belief-of-friends p "red")))
               (make-person (person-name p) "blue" (person-friends p))]
              [else p]))]
    (map person-update network)))


;ex 2

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
    (if IS-END-IN-BEGINNING-FRIENDS
        #true
        (connected-thru-friends? nw SAME-BELIEF-BEGINNING-FRIENDS p2))))
; belief-grabber : Network String -> Belief
; gets the belief of a person given their name and network
;; CE NEEDED
(define (belief-grabber nw p)
  (cond
    [(empty? nw) '()]
    [(cons? nw) (if (string=? p (person-name (first nw)))
                    (person-belief (first nw))
                    (belief-grabber (rest nw) p))]))

; get-friends : Network String -> [List-of String]
; gets the list of friends of a person given their name and network
;; CE NEEDED
(define (get-friends nw p)
  (cond
    [(empty? nw) '()]
    [(cons? nw) (if (string=? p (person-name (first nw)))
                    (person-friends (first nw))
                    (get-friends (rest nw) p))]))

; connected-thru-friends? : Network [List-of String] String -> Boolean
; checks to see if there is a connection between the friends and the end person (p2)
;; CE NEEDED
(define (connected-thru-friends? nw friends p2)
  (local [(define (can-reach-wrapper start-friend)
            (can-reach? nw start-friend p2))]
    (ormap can-reach-wrapper friends)))