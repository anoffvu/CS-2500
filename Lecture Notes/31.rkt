;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture31) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Welcome to the exclusive last lecture before Thanksgiving! Very exciting.
;; ANNOUNCEMENTS:
;; 1. The second exam is on Wednesday, November 28th from 6-9pm. We will release location details
;;    very shortly. As soon as we get a chance to figure those out.
;;    - The basic rules are the same: you can bring 1 sheet of paper, the cards we gave to you, and
;;      a writing utensil of your choice. That's it. If you lost your cards you are welcome to write
;;      their content on your cheat sheet. They are all posted on the course webpage.
;;    - Everything BEFORE lecture 30 is fair game for the exam (so, not generative recursion)
;;      - Lists
;;      - Abstractions (making your own and using pre-defined ones)
;;      - Scope and local
;;      - I/O
;;      - Multiple complex inputs
;;      - Trees
;;      - Graphs
;;    - This exam WILL be more difficult than the first exam.
;;    - The best way to prepare and get an idea of what the exam will look like is to take a look
;;      at previous exams (which are posted on the course webpage)
;; 2. There will be a review session on Sunday, November 25 which we will post about on Piazza.

;; Let's practice generative recursion!

;; Design the function chunkify that accepts a [List-of X] and a Nat, n, and returns a
;; [List-of [List-of X]] where each inner list is exactly n elements long (except perhaps
;; the last list which may contain fewer elements if the list cannot be split evenly)

;; chunkify : (X) [List-of X] Nat -> [List-of [List-of X]]
;; Chunk the list into lists of length n
(check-expect (chunkify '() 10) '())
(check-expect (chunkify (list 1 2 3 4 5 6 7) 3) (list (list 1 2 3) (list 4 5 6) (list 7)))
(check-expect (chunkify (list 1 2 3) 1) (list (list 1) (list 2) (list 3)))

;; Can we solve this problem structurally? Let's try to use the list template. The problem is that
;; the recursive call is not directly related to the result we want (except when n=1)
;; That is, for example, for (chunkify (list 1 2 3) 2) we want (list (list 1 2) (list 3))
;; but the recursive call gives us (list (list 2 3)) which is not the same at all.

;; We could try to solve it structurally using a natural number template. The problem is that the
;; recursive call is still not directly related to the result we want.
;; That is, for example, for (chunkify (list 1 2 3) 2) we want (list (list 1 2) (list 3))
;; but the recursive call gives us (list (list 1) (list 2) (list 3))

;; We could try using the template for multiple complex inputs. There are three ways to handle this:
;; 1. Handle one input and ignore the other (e.g. appending lists)
;; 2. Handle one input and then LATER handle the other
;; 3. Handle both inputs simultaneously.

;; What happens if we try to handle both inputs simultaneously? We have four cases:
;; 1. The list is empty and the Nat is zero
;; 2. The list is a cons and the Nat is zero
;; 3. The list is empty and the Nat is positive
;; 4. The list is a cons and the Nat is positive

;; Honestly I have no idea what to do in the second case? Using zero as an input is just nonsense.
;; You can't split a list into chunks of size zero. With an empty list we can just return '() but
;; with a cons we have to process it somehow...
;; In the 4th case we still have that problem of figuring out how the recursion relates to the
;; result we want.
;; (chunkify (list 1 2 3 4 5) 3) = (list (list 1 2 3) (list 4 5))
;; (chunkify (list 2 3 4 5) 2) = (list (list 2 3) (list 4 5))
;; Well we'd like to make those chunks of size n. But we have forgotten what n is because we
;; subtracted from it.

;; Okay so now that we went through all that we know that we really can't use structural recursion.

;; chunkify : (X) [List-of X] PosInt -> [List-of [List-of X]]
;; Chunk the list into lists of length n
;; TERMINATION: Every time we recur we remove elements from the list (either we remove n elements or
;; we remove all the elements), so the list is always smaller when we recur which will eventually
;; give us our base case of the empty list.
(check-expect (chunkify '() 10) '())
(check-expect (chunkify (list 1 2 3 4 5 6 7) 3) (list (list 1 2 3) (list 4 5 6) (list 7)))
(check-expect (chunkify (list 1 2 3) 1) (list (list 1) (list 2) (list 3)))
(define (chunkify lox n)
  (cond [(empty? lox) '()]
        [(cons? lox)
         (cons (take-first lox n)
               (chunkify (take-all-but lox n) n))]))

;; take-first : (X) [List-of X] Nat -> [List-of X]
;; Take the first n elements of the list (or return the list if it has fewer than n elements)
(check-expect (take-first '() 0) '())
(check-expect (take-first '() 10) '())
(check-expect (take-first (list "hello" "world") 0) '())
(check-expect (take-first (list 1 2 3 4 5) 2) (list 1 2))
(check-expect (take-first (list 1 2 3) 6) (list 1 2 3))
(define (take-first lox n)
  (cond [(and (empty? lox) (zero? n)) '()]
        [(and (empty? lox) (positive? n)) '()]
        [(and (cons? lox) (zero? n)) '()]
        [(and (cons? lox) (positive? n))
         (cons (first lox) (take-first (rest lox) (sub1 n)))]))

;; Can we simplify this? Well it looks like if we have an empty list we always return '() and
;; if we have zero we ALSO return '(). So here's a simplified version:

;; take-first.v2 : (X) [List-of X] Nat -> [List-of X]
;; Take the first n elements of the list (or return the list if it has fewer than n elements)
(check-expect (take-first.v2 '() 10) '())
(check-expect (take-first.v2 (list 1 2 3) 0) '())
(check-expect (take-first.v2 (list 1 2 3 4 5) 2) (list 1 2))
(check-expect (take-first.v2 (list 1 2 3) 6) (list 1 2 3))
(define (take-first.v2 lox n)
  (cond [(or (empty? lox) (zero? n)) '()]
        [(and (cons? lox) (positive? n))
         (cons (first lox) (take-first.v2 (rest lox) (sub1 n)))]))

;; BECCA: It is absolutely fine to re-work your functions after following the template and simplify
;; cases that are the same. However, if you think this makes the function more difficult to
;; understand, you should not do it. It's a judgement call.

;; BECCA: Professor Mislove noticed that we can actually take a non-empty list here since we
;; only use it in the cons case of our chunkify function. That gives you the following version:

;; take-first.v3 : (X) [NEList-of X] Nat -> [NEList-of X]
;; Take the first n elements of the list (or return the list if it has fewer than n elements)
(check-expect (take-first.v3 (list 4) 0) '())
(check-expect (take-first.v3 (list 5) 10) (list 5))
(check-expect (take-first.v3 (list 1 2 3) 0) '())
(check-expect (take-first.v3 (list "hello" "world") 1) (list "hello"))
(define (take-first.v3 nelx n)
  (cond [(and (empty? (rest nelx)) (zero? n)) '()]
        [(and (empty? (rest nelx)) (positive? n)) nelx]
        [(and (cons? (rest nelx)) (zero? n)) '()]
        [(and (cons? (rest nelx)) (positive? n))
         (cons (first nelx) (take-first.v3 (rest nelx) (sub1 n)))]))

;; And of course we can simplify this as well...

;; take-first.v4 : (X) [NEList-of X] Nat -> [NEList-of X]
;; Take the first n elements of the list (or return the list if it has fewer than n elements)
(check-expect (take-first.v4 (list 5) 10) (list 5))
(check-expect (take-first.v4 (list 1 2 3) 0) '())
(check-expect (take-first.v4 (list "hello" "world") 1) (list "hello"))
(define (take-first.v4 nelx n)
  (cond [(zero? n) '()]
        [(and (empty? (rest nelx)) (positive? n)) nelx]
        [(and (cons? (rest nelx)) (positive? n))
         (cons (first nelx) (take-first.v4 (rest nelx) (sub1 n)))]))

;; take-all-but : (X) [List-of X] Nat -> [List-of X]
;; Take all but the first n elements of the list (or return the empty list if it has fewer than n
;; elements)
(check-expect (take-all-but '() 0) '())
(check-expect (take-all-but '() 10) '())
(check-expect (take-all-but (list "hello") 0) (list "hello"))
(check-expect (take-all-but (list 1 2 3 4 5) 3) (list 4 5))
(check-expect (take-all-but (list 1 2 3) 6) '())
(define (take-all-but lox n)
  (cond [(and (empty? lox) (zero? n)) '()]
        [(and (empty? lox) (positive? n)) '()]
        [(and (cons? lox) (zero? n)) lox]
        [(and (cons? lox) (positive? n))
         (take-all-but (rest lox) (sub1 n))]))

;; Again, we can simplify...

;; take-all-but.v2 : (X) [List-of X] Nat -> [List-of X]
;; Take all but the first n elements of the list (or return the empty list if it has fewer than n
;; elements)
(check-expect (take-all-but.v2 '() 10) '())
(check-expect (take-all-but.v2 (list "hello") 0) (list "hello"))
(check-expect (take-all-but.v2 (list 1 2 3 4 5) 3) (list 4 5))
(check-expect (take-all-but.v2 (list 1 2 3) 6) '())
(define (take-all-but.v2 lox n)
  (cond [(or (empty? lox) (zero? n)) lox]
        [(and (cons? lox) (positive? n))
         (take-all-but.v2 (rest lox) (sub1 n))]))

;; BECCA: As with the last one, we can assume a non-empty list which is the version Professor
;; Mislove did so here's that:

;; take-all-but.v3 : (X) [NEList-of X] Nat -> [List-of X]
;; Take all but the first n elements of the list (or return the empty list if it has fewer than n
;; elements)
(check-expect (take-all-but.v3 (list "hi you") 0) (list "hi you"))
(check-expect (take-all-but.v3 (list 1) 10) '())
(check-expect (take-all-but.v3 (list "hello") 0) (list "hello"))
(check-expect (take-all-but.v3 (list 1 2 3 4 5) 3) (list 4 5))
(check-expect (take-all-but.v3 (list 1 2 3) 6) '())
(define (take-all-but.v3 lox n)
  (cond [(and (empty? (rest lox)) (zero? n)) lox]
        [(and (empty? (rest lox)) (positive? n)) '()]
        [(and (cons? (rest lox)) (zero? n)) lox]
        [(and (cons? (rest lox)) (positive? n))
         (take-all-but.v3 (rest lox) (sub1 n))]))

;; And of course we can simplify...

;; take-all-but.v4 : (X) [NEList-of X] Nat -> [List-of X]
;; Take all but the first n elements of the list (or return the empty list if it has fewer than n
;; elements)
(check-expect (take-all-but.v4 (list "hello") 0) (list "hello"))
(check-expect (take-all-but.v4 (list "goodbye") 1) '())
(check-expect (take-all-but.v4 (list 1 2 3 4 5) 3) (list 4 5))
(check-expect (take-all-but.v4 (list 1 2 3) 6) '())
(define (take-all-but.v4 lox n)
  (cond [(zero? n) lox]
        [(and (empty? (rest lox)) (positive? n)) '()]
        [(and (cons? (rest lox)) (positive? n))
         (take-all-but.v4 (rest lox) (sub1 n))]))