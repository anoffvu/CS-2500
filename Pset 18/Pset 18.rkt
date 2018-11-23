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

(check-expect (nat->string 123456789) "123456789")
(check-expect (nat->string 12345678) "12345678")
(check-expect (nat->string 1) "1")
(check-expect (nat->string 10) "10")

(define (nat->string nni)
  (local [(define LAST-DIGIT (digit->string (modulo nni 10)))
          (define ALL-OTHER-DIGITS (grab-all-other-digits nni))]
    (cond
      [(<= 0 nni 9) (digit->string nni)]
      [(> nni 9) (string-append ALL-OTHER-DIGITS LAST-DIGIT)])))


; grab-all-other-digits : NonNegative
; grabs all but the last digit in a number and turns it into a string

(check-expect (grab-all-other-digits 123456789) "12345678")
(check-expect (grab-all-other-digits 12345678) "1234567")
(check-expect (grab-all-other-digits 1) "")
(check-expect (grab-all-other-digits 10) "1")

(define (grab-all-other-digits nni)
  (cond
    [(<= 0 nni 9) ""]
    [(> nni 9) (nat->string (floor (/ nni 10)))]))


;ex 2

; string-split : String String -> [List-of String]
; Given a string and a delimiter (second string
; which is used to divide the string) splits the given string by that delimiter
; Termination: each time we iterate, we trim down the intial string until we reach the trivial base
; case of the string not containing any instances of the delimiter, in which case we return the rest
; of the string

(check-expect (string-split "hello o o o ll o hillo" "ll") (list "he" "o o o o " " o hi" "o"))
(check-expect (string-split "hello o o o ll o hillo" "not-in-string") (list "hello o o o ll o hillo"))
(check-expect (string-split "1212121212 12 12 1" "1") (list "" "2" "2" "2" "2" "2 " "2 " "2 " ""))
(check-expect (string-split "1 1 1 1 1 1 1    1" " ") (list "1" "1" "1" "1" "1" "1" "1" "" "" "" "1"))
(check-expect (string-split "hello%%bob%%%i%%%%am%%jack" "%%")
              (list "hello" "bob" "%i" "" "am" "jack"))
(check-expect (string-split "111111114" "1") (list "" "" "" "" "" "" "" "" "4"))
(check-expect (string-split "111111114" "4") (list "11111111" ""))

(define (string-split longstring delimiter)
  (if
   (not (string-contains? delimiter longstring))
   (list longstring)
   (cons (first (cut-first-segment longstring delimiter 0))
         (string-split (second (cut-first-segment longstring delimiter 0)) delimiter))))


; cut-first-segment : String String NonNegativeInteger -> [List-of String]
; Cuts the string into 2 parts: the string before the delimiter and the string after the delimiter
; the string must have a delimiter in it as that is the only case where we call this function

(check-expect (cut-first-segment "hello" "ll" 0) (list "he" "o"))
(check-expect (cut-first-segment "hello" "o" 0) (list "hell" ""))
(check-expect (cut-first-segment "hellollo" "ll" 0) (list "he" "ollo"))
(check-expect (cut-first-segment "llllllllll" "ll" 0) (list "" "llllllll"))
(check-expect (cut-first-segment "ollllllllll" "ll" 0) (list "o" "llllllll"))

(define (cut-first-segment longstring delimiter look-index)
  (local [(define DELIMITER-LENGTH (string-length delimiter))
          
          ; grab-segment-at-index : NonNegativeInteger -> String
          ; grabs a string from the longstring that is the same size as the delimiter
          ; at the given index
          ; for the checkexpects, assume the longstring is "hello123" and the delimiter is "00"
          ; given 3, returns "lo"
          ; given 0, returns "he"
          ; given 6, returns "23"
          (define (grab-segment-at-index index)
            (substring longstring index (+ index DELIMITER-LENGTH)))]
    (if (string=? (grab-segment-at-index look-index) delimiter)
        (list (substring longstring 0 look-index)
              (substring longstring (+ look-index DELIMITER-LENGTH) (string-length longstring)))
        (cut-first-segment longstring delimiter (+ look-index 1)))))
