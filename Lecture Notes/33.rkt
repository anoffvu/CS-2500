;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture33) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS
;; 1. You have an exam tonight. From 6-9pm. It's gonna be super fun! I'm excited.
;; 2. ISEC's electrical room flooded which is causing all kinds of problems. We have moved office
;;    hours today to 366 WVH (the calendar is up to date). We may also have to rearrange the exam
;;    rooms. We will let you know whether this is the case at 3pm today.


;; Today we will be doing some practice problems. These are the kinds of problems you might see on
;; an exam.

;; 1. Design a function which determines if every number in a list is bigger than a given number.

;; STRATEGY: Use andmap since you want to check if something is true for every element of a list.

;; all-bigger? : [List-of Number] Number -> Boolean
;; Returns #true if all the elements in the list are bigger than the given number
(check-expect (all-bigger? '() 3) #true)
(check-expect (all-bigger? (list 1 2 3) 0) #true)
(check-expect (all-bigger? (list 1 2 3) 1) #false)
(check-expect (all-bigger? (list 1 2 3) 2) #false)
(define (all-bigger? lon n)
  (local [;; Number -> Boolean
          ;; Is this number bigger than n?
          (define (is-bigger? num) (> num n))]
    (andmap is-bigger? lon)))

;; 2. Design a function which negates a predicate. (This one's tougher)

;; negate-pred : (X) [X -> Boolean] -> [X -> Boolean]
;; Produces a function that produces the opposite of the predicate
(check-expect (local [(define my-even? (negate-pred odd?))]
                (andmap my-even? (list 0 2 4 6 8))) #true)
(check-expect (local [(define not-string? (negate-pred string?))]
                (list (not-string? 100) (not-string? "hello")))
              (list #true #false))
(define (negate-pred predicate)
  (local [;; negated : X -> Boolean
          ;; Returns the opposite of what the predicate would return
          (define (negated x) (not (predicate x)))]
    negated))

;; Question from a student: Is this even in scope of the exam? Yup. Functions are values.
;; There is no reason you cannot design a function that produces a function since it
;; is just a value.

;; 3. Consider the following data definitions:

;; A BasePair is one of:
;; - "G"
;; - "A"
;; - "T"
;; - "C"
;; and represents a single base pair in DNA

;; A Sequence is a [List-of BasePair]
;; and represents a sequence of DNA

;; Design the function num-matches that accepts two Sequences and returns the number of places
;; where they contain the same base pair.

;; STRATEGY: We have multiple complex inputs so we have several options:
;; 1. Cross-product (recur on both simultaneously)
;; 2. Recur on one then on the other
;; 3. Ignore one of the inputs and recur on the other

;; We will use the cross-product template for two lists.

;; num-matches : Sequence Sequence -> Nat
;; Returns the number of places where the sequences contain the same base pair
(check-expect (num-matches '() '()) 0)
(check-expect (num-matches '() (list "G" "A" "T" "C")) 0)
(check-expect (num-matches (list "G" "A" "T" "C") '()) 0)
(check-expect (num-matches (list "G" "C" "T" "A") (list "G" "A" "T" "C")) 2)
(define (num-matches s1 s2)
  (cond [(and (empty? s1) (empty? s2)) 0]
        [(and (empty? s1) (cons? s2)) 0]
        [(and (cons? s1) (empty? s2)) 0]
        [(and (cons? s1) (cons? s2))
         (if (string=? (first s1) (first s2))
             (add1 (num-matches (rest s1) (rest s2)))
             (num-matches (rest s1) (rest s2)))]))

;; And of course we can simplify

;; num-matches.v2 : Sequence Sequence -> Nat
;; Returns the number of places where the sequences contain the same base pair
(check-expect (num-matches.v2 '() '()) 0)
(check-expect (num-matches.v2 '() (list "G" "A" "T" "C")) 0)
(check-expect (num-matches.v2 (list "G" "A" "T" "C") '()) 0)
(check-expect (num-matches.v2 (list "G" "C" "T" "A") (list "G" "A" "T" "C")) 2)
(define (num-matches.v2 s1 s2)
  (cond [(or (empty? s1) (empty? s2)) 0]
        [(and (cons? s1) (cons? s2))
         (if (string=? (first s1) (first s2))
             (add1 (num-matches.v2 (rest s1) (rest s2)))
             (num-matches.v2 (rest s1) (rest s2)))]))

;; 4. Without using append, define the function append-sequences that accepts two Sequences and
;; computes the sequence of the first followed by the second

;; STRATEGY: We have multiple complex inputs so we have several options:
;; 1. Cross-product (recur on both simultaneously)
;; 2. Recur on one then on the other
;; 3. Ignore one of the inputs and recur on the other

;; We will recur on one and ignore the other since we don't really care what the second list is,
;; we just want to stick it at the end. We can recur using our list template OR using a list
;; abstraction. We will do this using a list template first but then we can transform to a list
;; abstraction.

;; append-sequence : Sequence Sequence -> Sequence
;; Appends the two sequences together (without using 'append')
(check-expect (append-sequence '() '()) '())
(check-expect (append-sequence '() (list "G" "A" "T" "C")) (list "G" "A" "T" "C"))
(check-expect (append-sequence (list "G" "A" "T" "C") '()) (list "G" "A" "T" "C"))
(check-expect (append-sequence (list "G" "A") (list "T" "C")) (list "G" "A" "T" "C"))
(define (append-sequence s1 s2)
  (cond [(empty? s1) s2]
        [(cons? s1) (cons (first s1) (append-sequence (rest s1) s2))]))

;; Let's transform.

;; append-sequence.v2 : Sequence Seqeunce -> Sequence
;; Appends the two sequences together (without using 'append')
(check-expect (append-sequence.v2 '() '()) '())
(check-expect (append-sequence.v2 '() (list "G" "A" "T" "C")) (list "G" "A" "T" "C"))
(check-expect (append-sequence.v2 (list "G" "A" "T" "C") '()) (list "G" "A" "T" "C"))
(check-expect (append-sequence.v2 (list "G" "A") (list "T" "C")) (list "G" "A" "T" "C"))
(define (append-sequence.v2 s1 s2)
  (foldr cons s2 s1))

;; Well that was shorter. On the exam we will tell you if/when you need to use a list abstraction.
;; Please read the instructions to be sure you use them where appropriate.

;; 5. Courses at Northeastern have a subect, a number, a title, and a set of prerequisites. Design
;; a data definition for a Course.

(define-struct course [subject num title prereqs])
;; A Course is a (make-course String PosInt String [List-of Course])
;; - where subject is the subject of the course
;; - num is the course number
;; - title is the title of the course
;; - and prereqs is a list of prerequisites

;; Note that we CANNOT have cycles here because you wouldn't be able to define such an example. In
;; this case that's actually good because a cycle wouldn't make sense. If course 1 requires course 2
;; and course 2 requires course 1, nobody could take either of those courses...

(define COURSE-FUNDIES1 (make-course "CS" 2500 "Fundamentals I" '()))
(define COURSE-DISCRETE (make-course "CS" 1800 "Discrete Structures" '()))
(define COURSE-FUNDIES2
  (make-course "CS" 2510 "Fundamentals II" (list COURSE-FUNDIES1)))
(define COURSE-OOD
  (make-course "CS" 3500 "Object Oriented Design" (list COURSE-FUNDIES2)))
(define COURSE-DD
  (make-course "CS" 3200 "Database Design" (list COURSE-DISCRETE COURSE-FUNDIES2)))
(define COURSE-SOFTWAREDEV
  (make-course "CS" 5200 "Software Development" (list COURSE-OOD COURSE-DD)))

;; course-template : Course -> ???
(define (course-template c)
  (... (course-subject c) ... (course-num c) ...
       (course-title c) ... (loc-template (course-prereqs c)) ...))

;; loc-template : [List-of Course] -> ???
(define (loc-template loc)
  (cond [(empty? loc) ...]
        [(cons? loc)
         (... (course-template (first loc)) ...
              (loc-template (rest loc)) ...)]))

;; Unless we specifically ask for it you don't really HAVE to write the list template on the exam.

;; 6. Given a Course, design a function that computes the total number of courses that you have to
;; take before you can take the given course.

;; STRATEGY: There are several ways you can do this. Let's do it in steps:
;; 1. Get all the pre-requisites for all the courses
;; 2. Get the UNIQUE pre-requisites
;; 3. Get the length of that list

;; how-many-prereqs? : Course -> Nat
;; How many courses do you have to take in order to take this course?
(check-expect (how-many-prereqs? COURSE-FUNDIES1) 0)
(check-expect (how-many-prereqs? COURSE-FUNDIES2) 1)
(check-expect (how-many-prereqs? COURSE-DD) 3)
(check-expect (how-many-prereqs? COURSE-SOFTWAREDEV) 5) ;; Only count F2 and F1 once!!!
(define (how-many-prereqs? c)
  (length (get-unique-courses (get-all-prereqs c))))

;; get-unique-courses : [List-of Course] -> [List-of Course]
;; Get only the unique courses in the list
(check-expect (get-unique-courses '()) '())
(check-expect (get-unique-courses (list COURSE-FUNDIES1 COURSE-FUNDIES2))
              (list COURSE-FUNDIES1 COURSE-FUNDIES2))
(check-expect
 (get-unique-courses (list COURSE-FUNDIES1 COURSE-FUNDIES2 COURSE-FUNDIES1 COURSE-OOD))
 (list COURSE-FUNDIES2 COURSE-FUNDIES1 COURSE-OOD))
(define (get-unique-courses loc)
  (local [;; Course [List-of Courses] -> [List-of Course]
          ;; Add this course to the list if it isn't there already
          (define (add-if-unique c sofar)
            (if (member? c sofar) sofar (cons c sofar)))]
    (foldr add-if-unique '() loc)))

;; get-all-prereqs : Course -> [List-of Course]
;; Get all the prerequisites (with duplicates) for the given course
(check-expect (get-all-prereqs COURSE-FUNDIES1) '())
(check-expect (get-all-prereqs COURSE-FUNDIES2) (list COURSE-FUNDIES1))
(check-expect (map course-num (get-all-prereqs COURSE-SOFTWAREDEV))
              (list 2500 2510 2500 1800 2510 3500 3200))
(define (get-all-prereqs c)
  (local [;; Course [List-of Course] -> [List-of Course]
          ;; Add this course's prerequisites to the list
          (define (add-prereqs crs sofar)
            (append (get-all-prereqs crs) sofar))]
    (foldr add-prereqs (course-prereqs c) (course-prereqs c))))

;; 7. Design the function majority which takes a non-empty list and returns the most frequent
;; element (if there are multiple it just returns one of them).

;; STRATEGY: We need to keep track of how many of each element there are so that we can remember
;; what we saw before.

;; A [Count X] is a (list X Nat)
;; and represents an element and a count of how many times it appears

(define COUNT1 (list "hello" 10))
(define COUNT2 (list 4 1))

;; count-temp : (X) [Count X] -> ???
(define (count-temp cx)
  (... (first cx) ... (second cx) ...))

;; majority : (X) [NEList-of X] -> X
;; Returns the most frequent element
(check-expect (majority (list "hello")) "hello")
(check-expect (majority (list 1 2 3 4 1 1 1)) 1)
(check-expect (majority (list 1 2 3 2 4 1 1 1 2 2 2)) 2)
(check-expect (local [(define my-majority (majority (list 1 2)))]
                (or (= my-majority 1) (= my-majority 2))) #true)
(define (majority nelx)
  (find-best-count (count-occurrences nelx)))

;; find-best-count : (X) [NEList-of [Count X]] -> X
;; Find the element with the largest count
(check-expect (find-best-count (list (list "hello" 10))) "hello")
(check-expect (find-best-count (list (list "a" 2) (list "b" 3) (list "c" 1))) "b")
(check-expect (local [(define my-best (find-best-count (list (list "a" 1) (list "b" 1))))]
                (or (string=? my-best "a") (string=? my-best "b"))) #true)
(define (find-best-count count-list)
  (local [;; better-count : [Count X] [Count X] -> [Count X]
          ;; Returns the Count with the highest Nat
          (define (better-count c1 c2)
            (if (> (second c2) (second c1)) c2 c1))
          
          ;; find-best-count/list : [NEList-of [Count X]] -> [Count X]
          ;; Find the Count with the highest Nat
          (define (find-best-count/list count-list)
            (foldr better-count (first count-list) (rest count-list)))]
    (first (find-best-count/list count-list))))

;; count-occurrences : (X) [NEList-of X] -> [NEList-of [Count X]]
;; Count the number of occurrences of each element in the list
(check-expect (count-occurrences (list "hello")) (list (list "hello" 1)))
(check-expect
 (count-occurrences (list 1 2 3 2 4 1 1 1 2 2 2))
 (list (list 1 4) (list 2 5) (list 3 1) (list 4 1)))
(define (count-occurrences nelx)
  (local [;; add-to-count-list : X [NEList-of [Count X]] -> [NEList-of [Count X]]
          ;; Add 1 to the count for this element
          (define (add-to-count-list x sofar)
            (cond [(empty? sofar) (list (list x 1))]
                  [(cons? sofar)
                   (if (same-element? (first sofar) x)
                       (cons (add1-to-count (first sofar))
                             (rest sofar))
                       (cons (first sofar)
                             (add-to-count-list (rest sofar))))]))

          ;; same-element? : [Count X] X -> Boolean
          ;; Is this a counter for the given element?
          ;; NOTE: I had to use equal? because I don't know what X is. Please don't use equal?
          ;; if you can avoid it.
          (define (same-element? cx x)
            (equal? (first cx) x))

          ;; add1-to-count : [Count X] -> [Count X]
          ;; Add 1 to the count for the given element
          (define (add1-to-count cx)
            (list (first cx) (add1 (second cx))))]
    (foldr add-to-count-list '() nelx)))

;; There's plenty of other ways to do this problem so if you did it a different way don't panic.