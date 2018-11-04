;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Midterm 1 study|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;prob 1
(define-struct answer [to the question])

(define EX1
  (make-answer "Lois" 42 (string=? "CS2500"
                                   (string-append "2500" "CS"))))
(define EX2
  (make-answer "Clark" 654 #false))

(define (check-it ans)
  (cond
    [(> (answer-the ans) 50) (answer-to ans)]
    [(not (answer-question ans)) "Greetings"]
    [else (string-append "Hello, "
                         (answer-to ans))]))

; Evaluate for (check-it EX1)
; ex1: string=? of "cs2500" is false because "CS2500? =/= "2500CS" 
; (not (answer-question ans)) evaluates to (not (false)) which is true
; it is (not #false) so it goes to the last condition
; the last case will append "Hello, " and (answer-to ans)

;prob 2
(define-struct sling [shot])
(define-struct high [land tops])
; A Singer is one of:
; - (make-sling Boolean)
; - (make-high Number String)
; - #false
; Intepretation not needed for this problem
(define SINGER-1 (make-sling #true))
(define SINGER-2 (make-high 12 "hi"))
(define SINGER-0 #false)

;prob 3
(define-struct image-tweet [message picture])
(define-struct retweet [other-tweet])
; A Tweet is one of:
; - String
; - (make-image-tweet String Image)
; - (make-retweet String)
; Interpretation: Represents a tweet, which is either a message (String),
; a message along with a picture (make-image-tweet), or a retweet of
; another message (make-retweet)

(define (tweet-temp t)
  (cond
    [(string? t) ...]
    [(image-tweet? t) ... (image-tweet-message t) ... (image-tweet-picture t)...]
    [(retweet? t) ... (retweet-other-tweet t) ...]))

(tweet->text "hello") -> "hello"
(tweet->text (make-image-tweet "hello" (rectangle 20 20))) -> "hello"
(tweet->text (make-retweet "hi")) -> "hi"

;prob 4
; A PayRecord is a (make-record String Paycheck)
(define-struct record [name salary])
; Interpretation: a record of a payment to an employee
; A Paycheck is one of
; - PositiveNumber
; - (make-bonus PositiveNumber NonNegativeNumber)
(define-struct bonus [base-pay extra])
; Interpretation: A paycheck is either just some number of dollars, or
; includes both an employee's normal pay and some extra (both in dollars)
Consider the following very badly-designed function:
; Returns the name of the employee with the greater paycheck
; PayRecord PayRecord -> String
(define (max-earner emp1 emp2)
  (if (< (record-salary emp1) (record-salary emp2))
      (record-name emp1)
      (record-name emp2)))

Find two bugs in this function. For each one, explain what the problem is, and give a
test case that does not pass (i.e., either fails or crashes) to demonstrate the problem.

the first if clause will return emp1 even if emp1 is smaller than emp2
(max-earner (make-record "max" 45) (make-record "stax" 50)) should return stax but instead returns max

the second clause will return emp2 if it is less than emp1 (according to the logic)
(max-earner (make-record "max" 45) (make-record "unstax" 40)) should return max but returns max

third option: there is no case for when they are equal to each other

;prob 5
(define-struct team [sled speed])
; sled-speed: DogSled -> Number
; returns the max speed of a DogSled

(define DOGSLED-1 (make-team (make-team (make-team (make-team "sled" 1) 2) 3) 4))
(define DOGSLED-2 (make-team (make-team (make-team (make-team "sled" 150) 2) 3) 4))
(check-expect (sled-speed DOGSLED-1) 4)
(check-expect (sled-speed DOGSLED-2) 100)

(define (sled-speed ds)
  (cond
    [(string? ds) 0]
    [(team? ds) (if
                 (> (max (team-speed ds) (sled-speed (team-sled ds))) 100)
                 100
                 (max (team-speed ds) (sled-speed (team-sled ds))))]))

;; test 2016
;prob 1
(define-struct roof (top))
(define-struct free (way speech))
; A Cowboy is one of:
; -- (make-roof String)
; -- (make-free Boolean Number)
; intepretation not needed
Provide three data examples for Cowboy
(define COWBOY-1 (make-roof "hey"))
(define COWBOY-2 (make-free #true 12))
(define COWBOY-3 (make-free #false 21))

;prob 2

(define-struct brick (wall))
(define-struct fast (lane car))
; A Silly is one of:
; -- Integer
; -- (make-brick Number)
; -- (make-fast String Number)
; intepretation not needed
Which of the following are instances of Silly:
(define ex1 (make-brick "wall")) no because this is a (make-brick String) but the definition calls for a (make-brick Number)
(define ex2 3.14) no because it is a number and the definition calls for an integer
(define ex3 (make-fast "hello" 3.14)) yes because it is a (make-fast String Number) which is what it calls for
Explain why they do/do not belong to Silly

;prob 3
--[------](---------)[----------------->
  0     13          22
and this function signature:
; Nonnegative-Numbers -> String
(define (f x)
...)
(a) Develop test cases for f, assuming it identifies its inputs as
”low”, ”medium”, or ”high” according to the above interval diagram.
(f 0) -> "low"
(f 6) -> "low"
(f 13) -> "low"
(f 13.01) -> "medium"
(f 17) -> "medium"
(f 21.99) -> "medium"
(f 22) -> "high"
(f 29) -> "high"
(b) Develop the template for f, assuming it distinguishes its inputs
according to the above interval diagram.

(define (f-temp num)
  (cond
    [(<= num 13) ...]
    [(< num 22) ...]
    [(>= num 22) ...]))

;prob 4

(define-struct tall (order date))
(define-struct short (stop))
; A StockOrder is one of:
; -- PositiveNumber
; -- (make-tall Number String)
; -- (make-short Number)
Develop a template for gigi, a function with this header: OUTDATED THIS MIGHT BE WRONG
; StockOrder -> Boolean
; 
(define (gigi s)
#true)
(define (gigi-temp so)
  (cond [(number? so) ...]
        [(tall? so) ... (tall-order so) ... (tall-date so)...]
        [(short? so) ... (short-stop so) ...]
        [else #false]))

;prob 5

(define-struct listing (name price more))
; A DB is one of:
; -- #false
; -- (make-listing String Number DB)
; interpretation a sequence of realty listings
Design look. The function consumes a DB and a String. Its
result is the first number associated with the given String in the
given DB. If it can’t find the String, it produces -1.

;look: DB String -> Number
;returns the value associated with that string in a DB

(look (make-listing "one" 1 (make-listing "two" 2 (make-listing "three" 3 #false))) "one") -> 1
(look (make-listing "one" 1 (make-listing "two" 2 (make-listing "three" 3 #false))) "two") -> 2
(look (make-listing "one" 1 (make-listing "two" 2 (make-listing "three" 3 #false))) "hi") -> -1

(define (look db s)
  (cond [(boolean? db) -1]
        [(listing? db) (if
                        (string=? (listing-name db) s)
                        (listing-price db)
                        (look (listing-more db) s))]))












