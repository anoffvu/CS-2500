;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pset 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Exercise 1

; An Inch is a positive real number from (0, inf)
; Interpretation : a customary unit of measurement of distance
; Examples:
(define INCH-10 10)
(define INCH-20 20)

#;
(define inch-temp i
  ...i...)

; A Foot is a positive real number from (0, inf)
; Interpretation : a customary unit of measurement of distance
; Examples:
(define FOOT-10 10)
(define FOOT-20 20)

#;
(define foot-temp f
  ...f...)

; A Yard is a positive real number from (0, inf)
; Interpretation : a customary unit of measurement of distance
; Examples:
(define YARD-10 10)
(define YARD-20 20)

#;
(define YARD-temp y
  ...y...)

; convert-to-inches : Number Number Number -> Inch
; takes 3 inputs (Yards, Feet, Inches) and returns the total distance in inches

(check-expect (convert-to-inches 4 1 3) 159)
(check-expect (convert-to-inches 1 1 0) 48)

(define (convert-to-inches yards feet inches)
  (+ (* yards 36) (* feet 12) inches))

;; Exercise 2
; A Diagon is a real number from (0, inf)
; Interpretation : A Diagon is a make believe currency
; Examples:
(define DIAGON-100 100)
(define DIAGON-10 10)

#;
(define diagon-temp d
  ...d...)

; A Pound is a real number from (0, inf)
; Interpretation : A Harry Potter currency
; Examples :
(define POUND-20 20)
(define POUND-10 10)

#;
(define pound-temp p
  ...p...)

; diagon-convert-discount : Diagon -> Pound
; converts a number by a ratio, a flat fee is assessed when pound < 100

(check-expect (diagon-convert-discount 8) 1)
(check-expect (diagon-convert-discount 13) 2)
(check-expect (diagon-convert-discount 100) 20)
(check-expect (diagon-convert-discount 125) 25)

(define (diagon-convert-discount pounds)
  (cond
    [(< pounds 100)
     (/ (- pounds 3) 5)]
    [else (/ pounds 5)]))

;; Exercise 3
; A PixelColor is a number from (0, 256)
; Interpetation: a number to depict the value of a color
; Example:
(define PIXEL-COLOR-20 20)
(define PIXEL-COLOR-150 150)

#;
(define pixel-color-temp pc
  ...pc...)

; grayscale : PixelColor -> String
; receives a PixelColor and returns an image of the corresponding shade of gray
(check-expect (grayscale 0) "black")
(check-expect (grayscale 1) "black")
(check-expect (grayscale 20) "black")
(check-expect (grayscale 31) "black")
(check-expect (grayscale 32) "gray")
(check-expect (grayscale 33) "gray")
(check-expect (grayscale 100) "gray")
(check-expect (grayscale 239) "gray")
(check-expect (grayscale 240) "white")
(check-expect (grayscale 241) "white")
(check-expect (grayscale 250) "white")

(define (grayscale pc)
  (cond
    [(< pc 32) "black"]
    [(< pc 240) "gray"]
    [(< pc 256) "white"]))
    

;; Exercise 4

; A WordPlayOperation is one of:
; - "first"
; - "last"
; - "hyphenate"
; Interpretation: strings to dictate what a operation a function should do
; Examples:
(define WORDPLAY-OPERATION-FIRST "first")
(define WORDPLAY-OPERATION-LAST "last")
(define WORDPLAY-OPERATION-HYPHENATE "hyphenate")

#;
(define (word-play-operation-temp op)
  (cond
    [(string=? WORDPLAY-OPERATION-FIRST)...]
    [(string=? WORDPLAY-OPERATION-LAST)...]
    [(string=? WORDPLAY-OPERATION-HYPHENATE)...]))

; wordplay : WordPlayOperation String String String -> String
; takes an operation and series of strings and returns a manipulated string
(check-expect (wordplay "first" "a" "b" "c") "a")
(check-expect (wordplay "last" "a" "b" "c") "c")
(check-expect (wordplay "hyphenate" "a" "b" "c") "a-b-c")
(check-expect (wordplay "first" "b" "c" "c") "b")
(check-expect (wordplay "last" "hi" "hello" "hehe") "hehe")
(check-expect (wordplay "hyphenate" "hy" "phen" "ate") "hy-phen-ate")

(define (wordplay op string0 string1 string2)
  (cond
    [(string=? op WORDPLAY-OPERATION-FIRST)
     string0]
    [(string=? op WORDPLAY-OPERATION-LAST)
     string2]
    [(string=? op WORDPLAY-OPERATION-HYPHENATE)
     (string-append string0 "-" string1 "-" string2)]))


