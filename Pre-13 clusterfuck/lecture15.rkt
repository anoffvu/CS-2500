;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture15) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS
;; 1. Exam is on Thursday. If you have a conflict you should have already heard from me (Becca)
;;    about where and when you are taking it. If you are going to take it in the DRC you should
;;    have already talked to me (Becca) about it.
;; 2. Exam room locations are posted on Piazza so please go there for more information on the
;;    Thursday night locations.
;; 3. You can bring the following to the exam: 3 physical informational cards + 1 cheatsheet,
;;    double-sided OR 1 cheatsheet including the information on the cards. You cannot print out
;;    the cards and bring an additional cheatsheet.
;; 4. Homework 8 is due Monday, October 15th.
;; 5. On Tuesday, October 16th you will get a new partner which will be for homeworks 10 onward.
;;    That means that homework 9 (due Thursday, October 18th) is with your CURRENT partner.

;; A ListofStrings (LoS) is one of:
;; - '()
;; - (cons String LoS)
(define LOS0 '())
(define LOS1 (cons "Becca" (cons "Alan" (cons "Nate" '()))))

;; los-template : LoS -> ???
(define (los-template los)
  (cond [(empty? los) ...]
        [(cons? los) (... (first los) ... (los-template (rest los)) ...)]))

;; prefix-with-from : LoS -> LoS
;; Prefix every string with "from: "
(check-expect (prefix-with-from LOS0) LOS0)
(check-expect
 (prefix-with-from LOS1)
 (cons "from: Becca" (cons "from: Alan" (cons "from: Nate" '()))))
(define (prefix-with-from los)
  (cond [(empty? los) los]
        [(cons? los) (cons (string-append "from: " (first los))
                           (prefix-with-from (rest los)))]))

;; prefix-with-to : LoS -> LoS
;; Prefix every string with "to: "
(check-expect (prefix-with-to LOS0) LOS0)
(check-expect
 (prefix-with-to LOS1)
 (cons "to: Becca" (cons "to: Alan" (cons "to: Nate" '()))))
(define (prefix-with-to los)
  (cond [(empty? los) los]
        [(cons? los) (cons (string-append "to: " (first los))
                           (prefix-with-to (rest los)))]))

;; These two functions look VERY VERY similar. In this course we try to avoid repeating code as
;; this gives us twice the opportunities to make mistakes and does not make it easy for us to
;; go back and edit the code if we need to refine it. Is there a way for us to merge these two
;; functions so we don't have repeated code?

;; To do this we use ABSTRACTION:
;; 1. Find all the differences in the functions
;; 2. Design a function that takes those differences in as parameters
;; 3. Rewrite your original functions to use the abstraction

;; prefix-with : LoS String -> LoS
;; Prefix every string in the list with the given string
(check-expect (prefix-with LOS0 "doggo") LOS0)
(check-expect
 (prefix-with LOS1 "hello ")
 (cons "hello Becca" (cons "hello Alan" (cons "hello Nate" '()))))
(define (prefix-with los str)
  (cond [(empty? los) los]
        [(cons? los) (cons (string-append str (first los))
                           (prefix-with (rest los) str))]))

;; prefix-with-from.v2 : LoS -> LoS
;; Prefix every string in the list with "from: "
(check-expect (prefix-with-from.v2 LOS0) LOS0)
(check-expect
 (prefix-with-from.v2 LOS1)
 (cons "from: Becca" (cons "from: Alan" (cons "from: Nate" '()))))
(define (prefix-with-from.v2 los)
  (prefix-with los "from: "))

;; prefix-with-to.v2 : LoS -> LoS
;; Prefix every string in the list with "to: "
(check-expect (prefix-with-to.v2 LOS0) LOS0)
(check-expect
 (prefix-with-to.v2 LOS1)
 (cons "to: Becca" (cons "to: Alan" (cons "to: Nate" '()))))
(define (prefix-with-to.v2 los)
  (prefix-with los "to: "))

;; Let's do another one!

;; A ListofNumbers (LoN) is one of:
;; - '()
;; - (cons Number LoN)
(define LON0 '())
(define LON1 (cons 4 (cons 1 (cons 16 '()))))


;; lon-template : LoN -> ???
(define (lon-template lon)
  (cond [(empty? lon) ...]
        [(cons? lon) (... (first lon) ... (lon-template (rest lon)) ...)]))

;; sqrt-of-all : LoN -> LoN
;; Take the square root of each number in the list
(check-expect (sqrt-of-all LON0) LON0)
(check-expect (sqrt-of-all LON1) (cons 2 (cons 1 (cons 4 '()))))

(define (sqrt-of-all lon)
  (cond
    [(empty? lon) lon]
    [(cons? lon) (cons (sqrt (first lon))
                       (sqrt-of-all (rest lon)))]))


(define (sqrt-of-all lon)
  (cond [(empty? lon) lon]
        [(cons? lon) (cons (sqrt (first lon)) (sqrt-of-all (rest lon)))]))

;; sqr-of-all : LoN -> LoN
;; Returns the square of each number in the list
(check-expect (sqr-of-all LON0) LON0)
(check-expect (sqr-of-all LON1) (cons 16 (cons 1 (cons (sqr 16) '()))))
(define (sqr-of-all lon)
  (cond [(empty? lon) lon]
        [(cons? lon) (cons (sqr (first lon)) (sqr-of-all (rest lon)))]))

;; Let's use ABSTRACTION to combine these functions. What is the difference
;; between them? The difference is the function we call on the first number
;; in the list. How can we give this as an input to our abstract function?

;; It turns out we can do this, but not in BSL. So henceforth we will be
;; using ISL which you probably already knew because this file is written in
;; ISL. In any case, CONGRATULATIONS!!! You should use ISL starting with
;; assignment 9.

;; So the key thing to note here is that FUNCTIONS ARE DATA. This is a crazy
;; concept so if you're struggling with it that's pretty normal. But having
;; functions as values allows us to abstract the above functions since now we
;; can input the function sqr or sqrt to get the functions we want.

;; How do we write a signature for such a function? You CANNOT write "Operation"
;; or "Function" because those types are not defined. You know what types ARE
;; defined? Numbers! All the functions we are currently interested in take a Number
;; and return a Number. To see how this looks in a signature look below. We basically
;; take the signature for the function we're inputting and stick it in square brackets
;; in the signature for our function.

; do-to-all: LoN [Number -> Number] -> LoN
; performs an operation on all the items in a list

(check-expect (do-to-all LON0 add1) LON0)
(check-expect (do-to-all LON1 sqrt) (cons 2 (cons 1 (cons 4 '()))))

(define (do-to-all lon operation)
  (cond [(empty? lon) lon]
        [(cons? lon) (cons (operation (first lon))
                           (do-to-all (rest lon) operation))]))



(define (do-to-all lon operation)
  (cond [(empty? lon) lon]
        [(cons? lon) (cons (operation (first lon))
                           (do-to-all (rest lon) operation))]))

;; sqrt-of-all.v2 : LoN -> LoN
;; Take the square root of each number in the list
(check-expect (sqrt-of-all.v2 LON0) LON0)
(check-expect (sqrt-of-all.v2 LON1) (cons 2 (cons 1 (cons 4 '()))))
(define (sqrt-of-all.v2 lon) (do-to-all lon sqrt))

;; sqr-of-all.v2 : LoN -> LoN
;; Returns the square of each number in the list
(check-expect (sqr-of-all.v2 LON0) LON0)
(check-expect (sqr-of-all.v2 LON1) (cons 16 (cons 1 (cons (sqr 16) '()))))
(define (sqr-of-all.v2 lon) (do-to-all lon sqr))

;; Let's do another one!

;; A Posn is a (make-posn Number Number)
(define POSN1 (make-posn -3 4))
(define POSN2 (make-posn 0 0))

;; posn-template : Posn -> ???
(define (posn-template p)
  (... (posn-x p) ... (posn-y p) ...))

;; A ListofPosns (LoP) is one of:
;; - '()
;; - (cons Posn LoP)
(define LOP0 '())
(define LOP1 (cons POSN1 (cons POSN2 '())))

;; lop-template : LoP -> ???
(define (lop-template lop)
  (cond [(empty? lop) ...]
        [(cons? lop) (... (posn-template (first lop))
                          (lop-template (rest lop)) ...)]))

;; manhattan-dist-all : LoP -> LoN
;; Produces the manhattan distance to the origin for each Posn in the list
(check-expect (manhattan-dist-all LOP0) LOP0)
(check-expect (manhattan-dist-all LOP1) (cons 7 (cons 0 '())))
(define (manhattan-dist-all lop)
  (cond [(empty? lop) lop]
        [(cons? lop) (cons (manhattan-to-origin (first lop))
                           (manhattan-dist-all (rest lop)))]))

;; manhattan-to-origin : Posn -> Number
;; Find the manhattan distance to the origin
(check-expect (manhattan-to-origin POSN1) 7)
(check-expect (manhattan-to-origin POSN2) 0)
(define (manhattan-to-origin p)
  (+ (abs (posn-x p)) (abs (posn-y p))))

;; pythagorean-dist-all : LoP -> LoN
;; Produces the pythagorean distance to the origin for each Posn in the list
(check-expect (pythagorean-dist-all LOP0) LOP0)
(check-expect (pythagorean-dist-all LOP1) (cons 5 (cons 0 '())))
(define (pythagorean-dist-all lop)
  (cond [(empty? lop) lop]
        [(cons? lop) (cons (pythagorean-to-origin (first lop))
                           (pythagorean-dist-all (rest lop)))]))

;; pythagorean-to-origin : Posn -> Number
;; Find the pythagorean distance to the origin
(check-expect (pythagorean-to-origin POSN1) 5)
(check-expect (pythagorean-to-origin POSN2) 0)
(define (pythagorean-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; Tomorrow we will abstract these!