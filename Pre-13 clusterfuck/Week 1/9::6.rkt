;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9::6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SKY-WIDTH 200)
(define SKY-HEIGHT 300)
(define SUN (circle 40 "solid" "red"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue"))
(define (square-area sidelength)
  (sqr sidelength)
  )
(define SUNSET  (place-image
                 SUN
                 (/ SKY-WIDTH 2) SKY-HEIGHT
                 SKY
                 ))
(define MOON (circle 40 "solid" "white"))

; signature : Number -> Image
; purpose : puts a sun in the sky with a background
(define (sun-in-the-sky height)
  (place-image
   SUN
   (/ SKY-WIDTH 2)
   height
   SKY))
#|(place-image
 (circle 40 "solid" "red")
 115 100
 (place-image
  (circle 40 "solid" "red")
  100 300
  (rectangle 200 300 "solid" "blue")
  )
 )
|#


(define (add3 num)
  (+ num 3))

(add3 4)

; square-area : Number -> Number
; Finds the area of a square given the side length

; (square-area 10)

#|
(place-image
 SUN
 (* SKY-WIDTH 0.6) (* SKY-HEIGHT 0.4)
 SUNSET
 )
|#


;(animate sun-in-the-sky)
;animate always starts at 0

; draw-moon : Number -> Image
; Draw the moon on the sun at the given x-coordinate
#|
(define (draw-moon x-moon)
  (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN
                            (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            SKY
                            )))
(animate draw-moon)
|#
; gonna-get-an-A? : Number -> Boolean
; returns whether or not you're going to get an A given your numerical grade
;; Examples:
;; (gonna-get-an-A? 92) -> #true
;; (gonna-get-an-A? 80) -> #false
;; (gonna-get-an-A? 90) -> #true

;; v: check-expect
;; g: (check-expect function-call expected-result)
;; s: 

(check-expect (gonna-get-an-A? 92)
              #true)
(check-expect (gonna-get-an-A? 90)
              #true)
(check-expect (gonna-get-an-A? 80)
              #false)

(define (gonna-get-an-A? grade)
  (>= grade 90))

; V: cond
; G:
; (cond
;  [test-1 answer-1]
;  [test-2 answer-2]
;  ...
;  [test-n/else answer-n])

; sign : Number -> String: "positive" "negative" "zero"
; "positive" "negative" "zero"

(check-expect (sign 0) "zero")
(check-expect (sign 1) "positive")
(check-expect (sign -1) "negative")

(define (sign number)
  (cond
    [(> number 0) "positive"]
    [(< number 0) "negative"]
    [(= number 0) "zero"]
    ))

; grade-picker : Number -> String
; takes a numerical grade and returns the letter grade equivalent

(check-expect (grade-picker 105) "A")
(check-expect (grade-picker 19) "F")
(check-expect (grade-picker 50) "F")
(check-expect (grade-picker 70) "C")

(define (grade-picker number)
  (cond
    [(>= number 90) "A"]
    [(>= number 80) "B"]
    [(>= number 70) "C"]
    [(>= number 60) "D"]
    [(< number 60) "F"]
    ))
  

    


