;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |34|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ANNOUNCEMENTS
;; 1. Please do not discuss the exam as there are still people who are taking a makeup exam. It's
;;    not the same but it's similar so please don't reveal the details of the exam in person
;;    or via Piazza.
;; 2. There is no homework due tonight. You're welcome.
;; 3. We are grading the exam on Friday and we're hoping to give it back to you on Monday.
;; 4. There IS lab on Tuesday and there IS a lab quiz that day.

;; Let's talk some more about accumulators!

;; Design a function that reverses a list. Design this function structurally without using either
;; 'foldl' or 'append'

;; my-reverse : (X) [List-of X] -> [List-of X]
;; Reverse the order of the list
(check-expect (my-reverse '()) '())
(check-expect (my-reverse (list 1 2 3 4 5)) (list 5 4 3 2 1))
(define (my-reverse lox)
  (cond [(empty? lox) '()]
        [(cons? lox)
         (put-at-end (my-reverse (rest lox)) (first lox))]))

;; put-at-end : (X) [List-of X] X -> [List-of X]
;; Put the given element at the end of the list
(check-expect (put-at-end '() "hello") (list "hello"))
(check-expect (put-at-end (list 2 8 1) 7) (list 2 8 1 7))
(define (put-at-end lox x)
  (foldr cons (list x) lox))

;; How does this function do for efficiency? Try running the following:
#;(length (my-reverse (build-list 10000 add1)))
;; This takes a very noticeable amount of time (on my machine almost 1.5 seconds on average)

;; Design this function using an accumulator to make it more efficient.

;; What would our accumulator be?

;; Well we know our accumulator will take some element (starting with the first element)
;; and add it to the accumulator so far. How can we make use of that? Well let's think of
;; an example.

;; Suppose we call (my-reverse/a (list 1 2 3) some-base-case)
;; That will call (my-reverse/a (list 2 3) (some-function 1 some-base-case))
;; That is, it integrates the element into the accumulator.
;; That will call (my-reverse/a (list 3) (some-function 2 (some-function 1 some-base-case)))
;; And that will call
;; (my-reverse/a '() (some-function 3 (some-function 2 (some-function 1 some-base-case))))
;; At this point all we can really do is produce the accumulated value. So how do we turn
;; (some-function 3 (some-function 2 (some-function 1 some-base-case))) into (list 3 2 1)?
;; (cons          3 (cons          2 (cons          1 '())))

;; my-reverse.v2 : (X) [List-of X] -> [List-of X];; Reverse the order of the list
(check-expect (my-reverse.v2 '()) '())
(check-expect (my-reverse.v2 (list 1 2 3 4 5)) (list 5 4 3 2 1))
(define (my-reverse.v2 lox)
  (local [;; my-reverse/a : [List-of X] [List-of X] -> [List-of X]
          ;; Reverse the order of a list and add it to the reversed list so far
          ;; ACCUMULATOR: The elements of the list that have already been reversed
          (define (my-reverse/a lox sofar)
            (cond [(empty? lox) sofar]
                  [(cons? lox)
                   (my-reverse/a (rest lox) (cons (first lox) sofar))]))]
    (my-reverse/a lox '())))

;; Let's see if this is better. Try running the following:
#;(length (my-reverse.v2 (build-list 10000 add1)))
;; That's much faster! About 9 milliseconds on my machine.

;; Design the function f0ldl (the zero is just to distinguish it from actual foldl) which accepts a
;; function [X Y -> Y], a Y, and a [List-of X] and returns a Y which is the combined list

;; f0ldl : (X Y) [X Y -> Y] Y [List-of X] -> Y
;; Combine the list using the given function and base case
(check-expect (f0ldl + 0 (list 1 2 3)) (+ 3 (+ 2 (+ 1 0)))) ;; commutative so same as foldr
(check-expect (f0ldl cons '() (list "hello" "world")) (cons "world" (cons "hello" '()))) ;;reverse
(check-expect (f0ldl - 10 (list 1 2 3)) (- 3 (- 2 (- 1 10)))) ;; subtraction is not commutative
(check-expect (f0ldl * 5 '()) 5) ;; with the empty list we just return the base case
(define (f0ldl combiner base-case lox)
  (local [;; f0ldl/a : [List-of X] Y -> Y
          ;; Combine the list using combiner and the answer so far
          ;; ACCUMULATOR: The result of combining the elements of the list so far
          (define (f0ldl/a lox sofar)
            (cond [(empty? lox) sofar]
                  [(cons? lox)
                   (f0ldl/a (rest lox) (combiner (first lox) sofar))]))]
    (f0ldl/a lox base-case)))

;; BECCA: In our original code we passed in the base and the combiner but we noticed that we weren't
;; changing those values so we simplified. I've only included the simplified version here.
;; Also, when you write check-expects please calculate the actual answer. I only did it as above
;; so you could see how f0ldl was working.

;; Let's change gears a bit. Remember WAAAAAAAAAAAAAAAAAY back when we first learned 'define'? What
;; is special about 'define' as opposed to other functions? There are two DIFFERENT ways to use it:
(define MY-CONSTANT 100)
(define (my-function some inputs here) (* (+ some inputs) here))

;; The "function" define let's you bind names to values. Since functions are also values, it's
;; really doing the same thing in both cases, it just looks different.

;; Also, think about all the times we've had to create a function that uses some super simple
;; helper function. Wasn't that annoying?

;; Let's learn some new magic. Lambda.

;; (insert excited murmuring that happened at this point)

;; In order to do this we have to graduate to ISL+ (this would be more exciting if the notes were
;; somehow in ISL up to this point but I can't make the file be two different languages...)

;; Here's how you create a lambda expression:
#;(lambda (some inputs here ...) expression)
;; This creates a function without a name. That is, we haven't used 'define' to bind this function
;; to a name. How is that useful?

; (define (foo x) (+ x 3))
; is equal to λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ
; λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ
; λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ
; λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλF

(define (foo (lambda (x) (+ x 3))))

;; Here's an example

(define EXAMPLE1 (map (lambda (x) (+ x 3)) (list 1 2 3)))
;; This adds 3 every element in the list (list 1 2 3)
;; In the past we would have had to create a whole new function with a signature and purpose and so
;; on which adds 3 to a number. But adding 3 to a number is one of the most trivial things we can
;; do! So let's just use lambda instead.

;; Design the function short-msgs that takes a [List-of Strings] and returns only the messages that
;; have fewer than 14 characters.

(define SHORT-LENGTH 14)

;; short-msgs : [List-of String] -> [List-of String]
;; Returns only the messages that have fewer than 14 characters
(check-expect (short-msgs '()) '())
(check-expect (short-msgs (list "a" "hellohowareyoutoday" "bc")) (list "a" "bc"))
(define (short-msgs los)
  (filter (λ (message) (< (string-length message) SHORT-LENGTH)) los))

;; SO EASY NOW! ONLY ONE LINE! RADICAL. FAR OUT.
;; Sorry, slipped into another decade there for a minute.

;; Let's see our code from lecture 30 for 2500sort:

;; 2500sort : [List-of Number] -> [List-of Number]
;; Sort the numbers in ascending order
(check-expect (2500sort '()) '())
(check-expect (2500sort (list 5 2 6 1)) (list 1 2 5 6))
(define (2500sort lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (local [(define PIVOT (first lon))
                 (define LESS (elements-compared-to (rest lon) PIVOT <))
                 (define GREATER (elements-compared-to (rest lon) PIVOT >=))]
           (append (2500sort LESS)
                   (list PIVOT)
                   (2500sort GREATER)))]))

;; elements-compared-to : [List-of Number] Number [Number Number -> Boolean]
;; Filter the list by comparing elements with the given number using the given comparison
(check-expect (elements-compared-to '() 6 <) '())
(check-expect (elements-compared-to (list 1 2 3 4 5) 3 >) (list 4 5))
(define (elements-compared-to lon n comp)
  (local [;; good-compare? : Number -> Boolean
          ;; Is this element 'good' when compared to n?
          (define (good-compare? elem)
            (comp elem n))]
    (filter good-compare? lon)))

;; Let's simplify:

;; 2500sort.v2 : [List-of Number] -> [List-of Number]
;; Sort the numbers in ascending order
(check-expect (2500sort.v2 '()) '())
(check-expect (2500sort.v2 (list 5 2 6 1)) (list 1 2 5 6))
(define (2500sort.v2 lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (local [(define PIVOT (first lon))
                 (define LESS (filter (λ (n) (< n PIVOT)) lon))
                 (define GREATER (filter (λ (n) (>= n PIVOT)) lon))]
           (append (2500sort LESS)
                   (list PIVOT)
                   (2500sort GREATER)))]))

;; SO MUCH SHORTER! EASY TO READ! AMAZING!

;; So let's talk again about 'define'. It's obvious that (define X 3) binds a name to a value
;; but what about (define (foo x) (* x 2))? Well here's what happens behind the scenes:
(define foo (lambda (x) (* x 2)))

;; WHAT! CRAZY! WE LIED TO YOU THIS WHOLE TIME?!?
;; Well, we did tell you that functions are values. But yeah, basically (define (foo x) (* x 2))
;; is just a shorthand we taught you. It's all lambdas underneath.

;; Lambdas all the way down.