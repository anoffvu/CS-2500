;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |W3 Notes|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require 2htdp/image)

#|
; fundies-word : String -> String
; simple word processor
; dont do check expects for world problems

; wishlist
(define (fundies-word initialworld)
  (big-bang initialworld
    [to-draw draw-word]
    [on-key word-handle-key]))

(define BACKGROUND (square 200 "outline" "black"))
(define FONT-SIZE 24)
(define FONT-COLOR "black")

; draw-word : String -> Image
; returns an image that represents the entered text

(check-expect (draw-word "abc")
              (overlay
               (text "abc" FONT-SIZE FONT-COLOR)
               BACKGROUND))

(define (draw-word txt)
  (overlay
   (text txt FONT-SIZE FONT-COLOR)
   BACKGROUND))


;; ?? how do these signiatures work/ how to find them
; word-handle-backspace : String -> String
; process a backspace applied to the input string

(check-expect (word-handle-backspace "") "")
(check-expect (word-handle-backspace "abc") "ab")

;; how ifs work
(define (word-handle-backspace txt)
  (if
   (> (string-length txt) 0)
   (substring txt 0 (sub1 (string-length txt)))
   txt))



; ? word-handle-key : String KeyEvent -> String
; reflects keyboard inputs in the world state

(check-expect (word-handle-key "abc" "d") "abcd")
(check-expect (word-handle-key "" "p") "p")
(check-expect (word-handle-key "abc" "\b") "ab")
(check-expect (word-handle-key "" "\b") "")



(define (word-handle-key txt key)
  (cond
    [(key=? "\b" key) (word-handle-backspace txt)]
    ;sub1 is -1 you could also do (- string-length 1)
    [else (string-append txt key)]))


;; tangent back toward the eclipse program

(require 2htdp/image)

; posns
; V: make-posn
; G: (make-posn x-value y-value)
; S: you now have a posn with an x-value and y-value

; V: posn-x, posn-y
; G: (posn-x (make-posn x-value y-value))
;    (posn-y (make-posn x-value y-value))
; S: extra x/y from a make-posn
#|

(make-posn "foo" (circle 30 "solid" "red"))

(define SILLY-POSN (make-posn "foo" (circle 30 "solid" "red")))
(posn-y SILLY-POSN)
(posn-x SILLY-POSN)
|#

; A Position is a (make-posn Number Number)
; Interpretation: an x-t coordinate

(define POSITION-1-2 (make-posn 1 2))
(define POSITION-0-0 (make-posn 0 0))
(define POSITION-2-1 (make-posn 2 1))

#;
(define (position-temp p)
  ... (posn-x p) ... (posn-y p) ...)

; x-greater-than-y? : Position -> Boolean
; takes in a Position and returns true if x value of posn is greater than y
(check-expect (x-greater-than-y? POSITION-1-2) #false)
(check-expect (x-greater-than-y? POSITION-0-0) #false)
(check-expect (x-greater-than-y? POSITION-2-1) #true)
  

(define (x-greater-than-y? p)
  (> (posn-x p) (posn-y p)))


; add-10-to-x : Position -> Position
; adds 10 to the posn-x of a posn

(check-expect (add-10-to-x POSITION-1-2) (make-posn 11 2))
(check-expect (add-10-to-x POSITION-0-0) (make-posn 10 0))
(check-expect (add-10-to-x POSITION-2-1) (make-posn 12 1))

(define (add-10-to-x p)
  (make-posn (+ 10 (posn-x p)) (posn-y p)))
|#

;; Wednesday

(require 2htdp/image)

; A MoonPosition is a (make-posn RealNumber RealNumber)
; Interpretation: position of the moon
; Exaples:

(define MOON-POSITION-1 (make-posn 0 0))
(define MOON-POSITION-2 (make-posn 3 -3))

#;
(define (moon-position-temp mp)
  ... (posn-x mp) ... (posn-y mp) ...)

; constants
(define BACKGROUND (rectangle 300 200 "solid" "light blue"))
(define SUN (circle 25 "solid" "yellow"))
(define MOON (circle 25 "solid" "gray"))
(define SKY (overlay SUN BACKGROUND))


; main : MoonPosition -> MoonPosition
; draw an eclipse with a diagonal moon

(define (main initial-moon-position)
  (big-bang initial-moon-position
    [to-draw draw-eclipse]
    [on-tick move-moon]))

; draw-eclipse : MoonPosition -> Image
; given a moon position, draw the moon

(check-expect (draw-eclipse MOON-POSITION-1)
              (place-image MOON 0 0 SKY))

(define (draw-eclipse mp)
  (place-image MOON (posn-x mp) (posn-y mp) SKY))

; move-moon: MoonPosition -> MoonPosition
; moves the moon diagonally downwards

(check-expect (move-moon MOON-POSITION-1)
              (make-posn 1 1))
(check-expect (move-moon MOON-POSITION-2)
              (make-posn 4 -2))

(define (move-moon mp)
  (make-posn (+ 1 (posn-x mp)) (+ 1 (posn-y mp))))
             

; A SunMoonPosition is a (make-posn (make-posn Number Number) (make-posn Number Number))

; V (verb): define-struct 
; G (grammar): (define-struct name-o-struct [field1 field2 ...])
; S (semantics?): you have a new structure

; Constructor: (make-name-o-struct v1 v2)                 : Any Any -> (make-name-o-struct)
; Selectors:
; - (name-o-struct-field1 (make-name-o-struct v1 v2))     ; (make-name-o-struct) -> Any
; - (name-o-struct-field2 (make-name-o-struct v1 v2))     ; (make-name-o-struct) -> Any
;
; Predicate: (name-o-struct? v)                           ; Any -> Boolean


(define-struct moonsunpos [moonx moony sunx suny])
; A MoonSunPos is a (make-moonsunpos RealNumber RealNumber RealNumber RealNumber)
; Interpretation:
; - The first field is the x-position of the moon
; - The second field is the y-position of the moon
; - The third field is the x-position of the sun
; - The fourth field is the y-position of the sun
; Examples:

(define MOONSUNPOS-1 (make-moonsunpos 1 2 3 4))
#;
(define (moonsunpos-temp msp)
  ... (moonsunpos-moonx msp) ...
  ... (moonsunpos-moony msp) ...
  ... (moonsunpos-sunx msp) ...
  ... (moonsunpos-suny msp) ...)
#|
(define W1 (make-moonsunpos 1 1 3 3))
(moonsunpos-moonx W1)
(moonsunpos? W1)
(moonsunpos? (make-posn 1 1))
|#

; Design a function that takes a student structure
; and outputs the string "Dear FIRSTNAME LASTNAME (NUID),"

(define-struct student [fname lname nuid])
; A Student is a (make-student String String String)
; Interpretation: a student
; - First field is their first name
; - Second field is their last name
; - Third field is their NUID
; Examples:
(define STUDENT-1 (make-student "bob" "smith" "12345"))
(define STUDENT-2 (make-student "bab" "smoot" "12344"))

#;
(define (student-temp s)
  ... (student-fname s) ...
  ... (student-lname s) ...
  ... (student-nuid s) ...)


; salutation : Student -> String

(check-expect (salutation STUDENT-1) "Dear bob smith (12345)")
(check-expect (salutation STUDENT-2) "Dear bab smoot (12344)")

(define (salutation s)
  (string-append "Dear " (student-fname s) " " (student-lname s) " ("(student-nuid s) ")"))

; class 3

(require 2htdp/universe)
(require 2htdp/image)

(define-struct 2cars [x1 x2 v1 v2])

; A 2Cars is a (make-struct Number Number Number Number)
; Interpretation: position and velocity of 2 cards driving at eachother
; - x1 is the x-psoition of the first car
; - x1 is the x-psoition of the second car
; - v1 is the velocity of the first car in pixels/tick from the left
; - v1 is the velocity of the second car in pixels/tick from the left
; Examples:
(define 2CARS-1 (make-2cars 0 12 3 -3))

#;
(define (2cards-temp 2c)
  ... (2cars-x1 2c) ...
  ... (2cars-x2 2c) ...
  ... (2cars-v1 2c) ...
  ... (2cars-v2 2c) ...)

; main : 2Cars -> 2Cars
; simulates two cars "colliding"

(define (main initial-cars)
  (big-bang initial-cars
    [to-draw draw-cars]
    [on-tick move-cars]
    [stop-when cars-off-screen?]))
    

(define CAR-1 (rectangle 30 20 "solid" "blue"))
(define CAR-2 (rectangle 30 20 "solid" "red"))
(define BACKGROUND (rectangle 500 100 "solid" "white"))
(define CAR-Y 50)
(define ACCELERATION 0.5)


; draw-cars : 2Cars -> Image
; draws the cars

; (define 2CARS-1 (make-2cars 0 12 3 -3))
(check-expect (draw-cars 2CARS-1)
              (place-image
               CAR-1
               0 CAR-Y
               (place-image
                CAR-2
                12 CAR-Y
                BACKGROUND)))

(define (draw-cars 2c)
  (place-image
   CAR-1
   (2cars-x1 2c) CAR-Y
   (place-image
    CAR-2
    (2cars-x2 2c) CAR-Y
    BACKGROUND)))


; move-cars : 2Cars -> 2Cars
; move the cars relative to position/velocity/acceleration

(check-expect (move-cars 2CARS-1)
              (make-2cars 3 9 3.5 -3.5))

(define (move-cars 2c)
  (make-2cars
   (+ (2cars-x1 2c) (2cars-v1 2c))
   (+ (2cars-x2 2c) (2cars-v2 2c))
   (+ (2cars-v1 2c) ACCELERATION)
   (- (2cars-v2 2c) ACCELERATION)))

; cars-off-screen? : 2Cars -> Boolean
; tells whether either car is off the screen


(check-expect (cars-off-screen? 2CARS-1) #false)
(check-expect (cars-off-screen? (make-2cars 510 20 2 -7)) #true)
(check-expect (cars-off-screen? (make-2cars 20 -10 2 -7)) #true)

(define (cars-off-screen? 2c)
  (or
   (> (2cars-x1 2c) 500)
   (< (2cars-x2 2c) 0)))


              











































