;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname PS12) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Problem Set 12

(require 2htdp/batch-io)

(define EXBOLIVIONE-TEXT (read-words "/Users/krishmoran/Documents/exoblivione.txt"))

; Exercise 1

(define PUNCTUATION (list "," "." ";"))

; remove-punctuation : String -> String
; removes one instance of "," ";" or "." from the end of the input string, if it is there
(check-expect (remove-punctuation "hi.") "hi")
(check-expect (remove-punctuation "hello,") "hello")
(check-expect (remove-punctuation "hi;") "hi")
(check-expect (remove-punctuation "hi") "hi")
(define (remove-punctuation str)
  (local [; lastchar : String -> String
          ; returns the last character of the given string
          ; given "james", returns "s"
          ; given "lamelo;", returns ";"
          (define (lastchar str)
            (string-ith str (sub1 (string-length str))))]
    (if (member (lastchar str) PUNCTUATION)
        (substring str 0 (sub1 (string-length str)))
        str)))

; Exercise 2

; remove-punc-and-lcase : String -> String
; removes the ending punctuation from the given string, if it is there, and
; then converts it to its lowercase variant
(check-expect (remove-punc-and-lcase "HELLo.") "hello")
(check-expect (remove-punc-and-lcase "HELLo") "hello")
(check-expect (remove-punc-and-lcase "HeLLo;") "hello")
(check-expect (remove-punc-and-lcase "hello,") "hello")
(define (remove-punc-and-lcase str)
  ((compose string-downcase remove-punctuation) str))

; Exercise 3

; compound-word? : String -> Boolean
; determines if the given string contains "-"
(check-expect (compound-word? "hello") #false)
(check-expect (compound-word? "hell-o") #true)
(check-expect (compound-word? "hello-") #true)
(define (compound-word? str)
  (string-contains? "-" str))

; Exercise 4

; An [NEList-of X] (Non-Empty List of X) is one of:
; - (cons X '())
; - (cons X [NEList-of X])
 
; bin : [X X -> Boolean] [List-of X] -> [List-of [NEList-of X]]
; Bin lox by matches?
(check-expect (bin = '()) '())
(check-expect (bin = (list 2 3 4 2)) (list (list 2 2) (list 4) (list 3)))
(define (bin matches? lox)
  (local [; find-spot-for-x : X [List-of [NEList-of X]] -> [List-of [NEList-of X]]
          ; Find the spot for x
          (define (find-spot-for-x x bins)
            (find-spot x matches? bins))]
    (foldr find-spot-for-x '() lox)))
 
; find-spot : X [X X -> Boolean] [List-of [NEList-of X]] -> [List-of [NEList-of X]]
; Find where x belongs and place it in the appropriate list (or give it its own)
(check-expect (find-spot 2 = '()) (list (list 2)))
(check-expect (find-spot 2 = (list (list 3) (list 2) (list 4))) (list (list 3) (list 2 2) (list 4)))
(define (find-spot x matches? bins)
  (cond [(empty? bins) (list (list x))]
        [(cons? bins) (if (matches? x (first (first bins)))
                          (cons (cons x (first bins))
                                (rest bins))
                          (cons (first bins)
                                (find-spot x matches? (rest bins))))]))

; unique-compound-words : [List-of String] -> [List-of String]
; grabs all the unique compound words from a list
(check-expect (unique-compound-words (list "hi" "hello" "hi-" "hi-" "he-llo")) (list "he-llo" "hi-"))
(check-expect (unique-compound-words (list "hi" "hello" "hi-" "he-llo")) (list "he-llo" "hi-"))
(check-expect (unique-compound-words (list "hi" "hello")) '())
(check-expect (unique-compound-words (list "hi" "hello" "hi-" "hi-" "HI-""he-llo"))
              (list "he-llo" "hi-"))
(check-expect (unique-compound-words (list "hi" "hello" "hi-" "hi-" "HI-.""he-llo"))
              (list "he-llo" "hi-"))

(define (unique-compound-words los)
  (local [; mod-string=? : String String -> Boolean
          ; tests if both strings are equal, ignoring punctuation and capitalization
          ; given "hi" and "HI"; returns #true
          ; given "hi" and "hi."; returns #true
          ; given "hi" and "hello"; returns #false
          (define (mod-string=? s1 s2)
            (string=? (remove-punc-and-lcase s1) (remove-punc-and-lcase s2)))          
          ; takes out the compound words in a list
          ; given (list "hi-" "hello" "h-i"); returns (list "hi-" "h-i")
          (define COMPOUND-LIST (filter compound-word? los))]
    (map first (bin mod-string=? COMPOUND-LIST))))

(define UNIQUE-COMPOUND-WORDS (unique-compound-words EXBOLIVIONE-TEXT))

; Exercise 5

; A RawInput is a (list String String String String String String String)
; and represents the x1, y1, z1, x2, y2, z2, and confidence level of a line in 3d space
(define RAWINPUT-0 (list "0" "0" "0" "1" "1" "1" "0"))
(define RAWINPUT-1 (list "0" "0" "0" "10" "10" "10" "1"))
(define RAWINPUT-2 (list "0" "0" "0" "10" "10" "10" "0.5"))
#;
(define (rawinput-temp ri)
  ... (first ri) ... (second ri) ... (third ri) ... (fourth ri) ...
  (fifth ri) ... (sixth ri) ... (seventh ri) ...)

; A NumericInput is a (list Number Number Number Number Number Number [0, 1])
; and represents the x1, y1, z1, x2, y2, z2, and confidence level in the accuracy of the data
(define NUMERICINPUT-0 (list 0 0 0 1 1 1 0))
(define NUMERICINPUT-1 (list 0 0 0 10 10 10 1))
(define NUMERICINPUT-2 (list 4 3 2 -10 5 -1 0.5))
#;
(define (numericinput-temp ri)
  ... (first ni) ... (second ni) ... (third ni) ... (fourth ni) ...
  (fifth ni) ... (sixth ni) ... (seventh ni) ...)

; raw->numeric : RawInput -> NumericInput
; Convert raw input to numeric input
(check-expect (raw->numeric (list "0" "1" "2" "3" "4" "5" ".4")) (list 0 1 2 3 4 5 0.4))
(define (raw->numeric raw)
  (map string->number raw))

; read-in-segements : String -> [List-of NumericInput]
; Read in the line segments contained in s and map to numeric input
(define (read-in-segements s)
  (read-csv-file/rows s raw->numeric))

(define ALL-NUMERIC-INPUTS (read-in-segements "/Users/krishmoran/Documents/line-segments.csv"))

; filter-confidence: [List-of NumericInput] -> [List-of NumericInput]
; returns only those NumericInputs which have a confidence level of 0.2 or higher
(check-expect (filter-confidence (list NUMERICINPUT-0 NUMERICINPUT-1 NUMERICINPUT-2))
              (list NUMERICINPUT-1 NUMERICINPUT-2))
(check-expect (filter-confidence (list NUMERICINPUT-1 NUMERICINPUT-2))
              (list NUMERICINPUT-1 NUMERICINPUT-2))
(check-expect (filter-confidence (list NUMERICINPUT-0)) (list ))

(define (filter-confidence loni)
  (local [; conf-greater-than-.2? : NumericInput -> Boolean
          ; determines if the confidence level of a given NumericInput is greater than or equal to .2
          ; given NUMERICINPUT-0, return #false
          ; given NUMERICINPUT-1, return #true
          ; given NUMERICINPUT-2, return #true
          (define (conf-greater-than-.2? ni)
            (>= (seventh ni) .2))]
    (filter conf-greater-than-.2? loni)))


; Exercise 6

(define-struct linesegment [x1 y1 z1 x2 y2 z2])
; A LineSegment is a (make-linesegment [Number Number Number Number Number])
; represents 2 3D points which form a line segment
; x1 is the x coordinate of point 1
; y1 is the y coordinate of point 1
; z1 is the z coordinate of point 1
; x2 is the x coordinate of point 2
; y2 is the y coordinate of point 2
; z2 is the z coordinate of point 2

(define LINESEGMENT-0 (make-linesegment 0 0 0 1 1 1))
(define LINESEGMENT-1 (make-linesegment 0 0 0 5 5 5))
(define LINESEGMENT-2 (make-linesegment 5 5 5 1 1 1))
(define LINESEGMENT-3 (make-linesegment 5 5 5 0 0 0))
(define LINESEGMENT-4 (make-linesegment 0 0 0 25 25 25))
(define LINESEGMENT-5 (make-linesegment -1 -2 -3 1 5 10))

#;
(define (linesegment-temp ls)
  ... (linesegment-x1 ls) ... (linesegment-y1 ls) ... (linesegment-z1 ls)
  ... (linesegment-x2 ls) ... (linesegment-y2 ls) ... (linesegment-z2 ls) ...)

; Exercise 7

; ni->ls : NumericInput -> LineSegment
; converts the given NumericInput to a LineSegment
;(define NUMERICINPUT-2 (list 4 3 2 -10 5 -1 0.5))
(check-expect (ni->ls NUMERICINPUT-0) (make-linesegment 0 0 0 1 1 1))
(check-expect (ni->ls NUMERICINPUT-1) (make-linesegment 0 0 0 10 10 10))
(check-expect (ni->ls NUMERICINPUT-2) (make-linesegment 4 3 2 -10 5 -1))
(define (ni->ls ni)
  (make-linesegment (first ni) (second ni) (third ni) (fourth ni) (fifth ni) (sixth ni)))

; Exercise 8

; flip-ls : LineSegment -> LineSegment
; flips a LineSegment by swapping its two points
(check-expect (flip-ls LINESEGMENT-0) (make-linesegment 1 1 1 0 0 0))
(check-expect (flip-ls LINESEGMENT-1) (make-linesegment 5 5 5 0 0 0))
(check-expect (flip-ls LINESEGMENT-2) (make-linesegment 1 1 1 5 5 5))
(check-expect (flip-ls LINESEGMENT-3) (make-linesegment 0 0 0 5 5 5))
(check-expect (flip-ls LINESEGMENT-4) (make-linesegment 25 25 25 0 0 0))
(check-expect (flip-ls LINESEGMENT-5) (make-linesegment 1 5 10 -1 -2 -3))

(define (flip-ls ls)
  (make-linesegment
   (linesegment-x2 ls) (linesegment-y2 ls) (linesegment-z2 ls)
   (linesegment-x1 ls) (linesegment-y1 ls) (linesegment-z1 ls)))

; Exercise 9

; attached-at-first-point? : LineSegment LineSegment -> Boolean
; determines if the two given LineSegments have the same first point
(check-expect (attached-at-first-point? LINESEGMENT-0 LINESEGMENT-1) #true)
(check-expect (attached-at-first-point? LINESEGMENT-1 LINESEGMENT-2) #false)
(check-expect (attached-at-first-point? LINESEGMENT-2 LINESEGMENT-3) #true)
(check-expect (attached-at-first-point? LINESEGMENT-0 LINESEGMENT-3) #false)
(check-expect (attached-at-first-point? LINESEGMENT-4 LINESEGMENT-5) #false)

(define (attached-at-first-point? ls1 ls2)
  (and (= (linesegment-x1 ls1) (linesegment-x1 ls2))
       (= (linesegment-y1 ls1) (linesegment-y1 ls2))
       (= (linesegment-z1 ls1) (linesegment-z1 ls2))))

; Exercise 10

; bin-linesegs : [List-of LineSegment] -> [List-of [List-of LineSegment]]
; bins a list of line segments based on their first point
(check-expect (bin-linesegs
               (list LINESEGMENT-0 LINESEGMENT-1 LINESEGMENT-2 LINESEGMENT-3 LINESEGMENT-4))
              (list
               (list (make-linesegment 25 25 25 0 0 0))
               (list (make-linesegment 0 0 0 1 1 1) (make-linesegment 0 0 0 5 5 5)
                     (make-linesegment 0 0 0 25 25 25) (make-linesegment 0 0 0 5 5 5))
               (list (make-linesegment 1 1 1 0 0 0) (make-linesegment 1 1 1 5 5 5))
               (list (make-linesegment 5 5 5 1 1 1) (make-linesegment 5 5 5 0 0 0)
                     (make-linesegment 5 5 5 0 0 0))))
(check-expect (bin-linesegs (list LINESEGMENT-0))
              (list (list (make-linesegment 1 1 1 0 0 0))
                    (list (make-linesegment 0 0 0 1 1 1))))
(check-expect (bin-linesegs (list LINESEGMENT-0 LINESEGMENT-1))
              (list
               (list (make-linesegment 5 5 5 0 0 0))
               (list (make-linesegment 1 1 1 0 0 0))
               (list
                (make-linesegment 0 0 0 1 1 1)
                (make-linesegment 0 0 0 5 5 5))))

(define (bin-linesegs lols)
  (local [; creates a large list of the original list plus the list of "flipped" LineSegments
          (define FULL-LIST (append lols (map flip-ls lols)))]
    (bin attached-at-first-point? FULL-LIST)))

; Exercise 11

; num-of-pairs : [List-of X] -> Integer
; computes the number of pairs of elements in a given [List-of X]
(check-expect (num-of-pairs (list "hello" "hi")) 1)
(check-expect (num-of-pairs (list 1 2 4 5 6 2 1 2)) 28)
(check-expect (num-of-pairs (list #true #false #true #true)) 6)
(check-expect (num-of-pairs (list "test")) 0)

(define (num-of-pairs lox)
  (local [; n is the size of the list lox
          (define n (length lox))]
    (/ (* n (sub1 n)) 2)))

; Exercise 12

; num-of-all-possible-pairs : [List-of LineSegment] -> Number
; bins a list of LineSegments, counts the pairs in each bin,
; and returns the sum of all the number of pairs in all the bins.
(check-expect (num-of-all-possible-pairs (list LINESEGMENT-0)) 0)
(check-expect (num-of-all-possible-pairs (list LINESEGMENT-0 LINESEGMENT-1)) 1)
(check-expect (num-of-all-possible-pairs (list LINESEGMENT-0 LINESEGMENT-1 LINESEGMENT-2)) 3)
(check-expect (num-of-all-possible-pairs
               (list LINESEGMENT-0 LINESEGMENT-1 LINESEGMENT-2 LINESEGMENT-3)) 7)
(check-expect (num-of-all-possible-pairs
               (list LINESEGMENT-0 LINESEGMENT-1 LINESEGMENT-2 LINESEGMENT-3 LINESEGMENT-4)) 10)
(check-expect (num-of-all-possible-pairs
               (list LINESEGMENT-0 LINESEGMENT-1
                     LINESEGMENT-2 LINESEGMENT-3
                     LINESEGMENT-4 LINESEGMENT-5)) 10)
(define (num-of-all-possible-pairs lols)
  (local [(define BINNED-LIST (bin-linesegs lols))
          (define NUMBER-OF-PAIRS-LIST (map num-of-pairs BINNED-LIST))]
    (foldr + 0 NUMBER-OF-PAIRS-LIST)))

; Exercise 13

; ni-list->ls-list : [List-of NumericInput] -> [List-of LineSegment]
; converts the list of NumericInputs to a list of LineSegments

(check-expect (ni-list->ls-list
               (list (list 0 0 0 0 0 0 0.2) (list 0 0 0 0 0 0 0.3) (list 0 0 0 0 0 0 1)
                     (list 0 0 0 0 0 0 0.4)))
              (list (make-linesegment 0 0 0 0 0 0) (make-linesegment 0 0 0 0 0 0)
                    (make-linesegment 0 0 0 0 0 0)(make-linesegment 0 0 0 0 0 0)))

(define (ni-list->ls-list loni)
  (map ni->ls loni))

(define NUMBER-OF-PAIRS (num-of-all-possible-pairs (ni-list->ls-list
                                                    (filter-confidence ALL-NUMERIC-INPUTS))))





          
