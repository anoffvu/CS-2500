;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Pset 18|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;ex 1

; digit->string : [0, 9] -> String
; String representation of this digit
(check-expect (build-list 10 digit->string)
              (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(check-expect (digit->string 0) "0")

(define (digit->string d)
  (string (integer->char (+ 48 d))))

; nat->string : NonNegativeInteger -> String
; given a non-negative integer, returns its string form
; Termination: every time we recur, we get rid of the last digit, which will eventually
; find itself within our trivial case of [0,9] for any non-negative integer
; then we can just use digit->string to transform it to a string
; CEs NEEDED
(define (nat->string nni)
  (local [(define LAST-DIGIT (digit->string (modulo nni 10)))
          (define ALL-OTHER-DIGITS (grab-all-other-digits nni))]
  (cond
    [(<= 0 nni 9) (digit->string nni)]
    [(> nni 9) (string-append ALL-OTHER-DIGITS LAST-DIGIT)])))

; grab-all-other-digits : NonNegative
; grabs all but the last digits in a number
; CEs NEEDED
(define (grab-all-other-digits nni)
  (cond
    [(<= 0 nni 9) ""]
    [(> nni 9) (nat->string (floor (/ nni 10)))]))

;ex 2

; string-split : String String -> [List-of String]
; Given a string and a delimiter (second string
; which is used to divide the string) splits the given string by that delimiter

(define (string-split longstring delimiter)
  (local [(define DELIMITER-LENGTH (length delimiter))
          (define EXPLODED-DELIMITER (explode delmiter))]
    (cond
      [(
    
  




  