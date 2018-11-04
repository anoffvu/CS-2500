;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |W2 Notes|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
#|
(define SKY-WIDTH 600)
(define SKY-HEIGHT 400)
(define SUN (circle 25 "solid" "yellow"))
(define MOON (circle 25 "solid" "light blue"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
(define DARKSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "dark blue"))
(define DIMSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue"))

; draw-eclipse : Number -> Image
; draw the moon at the given x-coord, on a scene with the sun

(define (draw-moon x-moon)
  (cond
  [(= x-moon (/ SKY-WIDTH 2))
      (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) DARKSKY))]
  [(< (abs (- x-moon (/ SKY-WIDTH 2))) 50)
      (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) DIMSKY))]
  [else (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) SKY))]))
  
(animate draw-moon)

|#





; A NumericGrade is a real number in [0,100]
; Interpretation : a student's grade in the class
; Examples:
(define NUMERIC-GRADE-90 90)
(define NUMERIC-GRADE-35 35)
(define NUMERIC-GRADE-.8 0.8)

;(define (numeric-grade-temp ng)
;  ... ng ...)

; A StudentGrade is one of:
; - "A"
; - "B"
; - "C"
; - "D"
; - "F"
; Interpretation: a letter representation of a grade in the US
; Examples:
(define STUDENT-GRADE-A "A")
(define STUDENT-GRADE-B "B")
(define STUDENT-GRADE-C "C")
(define STUDENT-GRADE-D "D")
(define STUDENT-GRADE-F "F")

;(define (student-grade-temp sg)
;  (cond
;    [(string=? sg STUDENT-GRADE-A) ...]
;    [(string=? sg STUDENT-GRADE-B) ...]
;    [(string=? sg STUDENT-GRADE-C) ...]
;    [(string=? sg STUDENT-GRADE-D) ...]
;    [(string=? sg STUDENT-GRADE-F) ...]))
    

; A GPA is a real number in [0, 4]
; Interpretation: 
; Examples:
(define GPA-1.2 1.2)
(define GPA-4.0 4.0)
(define GPA-3.0 3.0)
(define GPA-2.0 2.0)
(define GPA-1.0 1.0)
(define GPA-0.0 0.0)


;(define (gpa-temp gpa)
; ...gpa...)

; grade->gpa : Student Grade -> GPA
; Converts from a letter to the GPA numeric equivalent

(check-expect (grade->gpa STUDENT-GRADE-A) GPA-4.0)
(check-expect (grade->gpa STUDENT-GRADE-B) GPA-3.0)
(check-expect (grade->gpa STUDENT-GRADE-C) GPA-2.0)
(check-expect (grade->gpa STUDENT-GRADE-D) GPA-1.0)
(check-expect (grade->gpa STUDENT-GRADE-F) GPA-0.0)

(define (grade->gpa sg)
  (cond
    [(string=? sg STUDENT-GRADE-A) 4.0]
    [(string=? sg STUDENT-GRADE-B) 3.0]
    [(string=? sg STUDENT-GRADE-C) 2.0]
    [(string=? sg STUDENT-GRADE-D) 1.0]
    [(string=? sg STUDENT-GRADE-F) 0.0]))

; 90
; 35
; 0.8

; num -> grade : NumericGrade -> StudentGrade


; A temperature is a positive real number from [0, inf]
; Interpretation : the temperature in Kelvin
; Examples
(define TEMPERATURE-20 20)
(define TEMPERATURE-0 0)

#;
(define (temperature-temp t)
  ... t ...)

; An MBTANonBus is one of...
; - "Green"
; - "Orange"
; - "Red"
; - "Blue"
; - "Purple"
; Interpretation : Names of the lines of non-bus MBTA vehicular modes of transportation

(define MBTA-NON-BUS-GREEN "Green")
#;
(define (mbta-non-bus-temp m)
  (cond
    [(string=? m MBTA-NON-BUS-GREEN) ...]
    []
    []
    []
    []))




; grade-picker : Number -> String
; takes a numerical grade and returns the letter grade equivalent
(define (grade-picker number)
  (cond
    [(>= number 90) "A"]
    [(>= number 80) "B"]
    [(>= number 70) "C"]
    [(>= number 60) "D"]
    [(< number 60) "F"]
    ))


;; THURSDAY

; You are running a car dealership.
; the dealership sells 4 types of cars: coupes, hatchbacks, sedans, & minivans
; Design the function num-doors that returns the number of doors each car has

; A car is one of...
; - "coupe"
; - "hatchback"
; - "sedan"
; - "minivan"
; Interpretation : a type of car sold at this dealership
; Examples :
(define CAR-COUPE "coupe")
(define CAR-HATCHBACK "hatchback")
(define CAR-SEDAN "sedan")
(define CAR-MINIVAN "minivan")

#;
(define (car-temp c)
  (cond
    [(string=? c CAR-COUPE) ...]
    [(string=? c CAR-HATCHBACK) ...]
    [(string=? c CAR-SEDAN) ...]
    [(string=? c CAR-MINIVAN) ...]))


; num-doors : Car -> PositiveInteger
; given a car style and returns the number of doors

(check-expect (num-doors CAR-COUPE) 2)
(check-expect (num-doors CAR-HATCHBACK) 5)
(check-expect (num-doors CAR-SEDAN) 4)
(check-expect (num-doors CAR-MINIVAN) 5)

(define (num-doors c)
  (cond
    [(string=? c CAR-COUPE) 2 ]
    [(string=? c CAR-HATCHBACK) 5]
    [(string=? c CAR-SEDAN) 4]
    [(string=? c CAR-MINIVAN) 5]))

#|
Notes:
Big Bang
World Programs

State: info that a program maintains and it can change over time
core independent data

Example:
very very very basic word processor
it needs to store text (before after and the screen)
when you press a key, it changes internal info
its state: all the text

Eclipse program
info it needed to maintain or change over time: x-moon (independent) , XXXX bg color (dependent) , moon on top? (dependent) SO XXXX
X MOON IS THE ONLY INFO NECESSARY FOR STATE

a traffic light (1 light, g -> y -> r)
State: color


anywhere you see WORLD
think about the state of the program

in a text editor, youd have a big world state in the formo of the large amount of text
in the traffice light it would be the color of the light

in big bang, when you call you 'give' the initial world and it returns the last world before it exits the world

|#


; A TrafficLight is one of:
; - "green"
; - "yellow"
; - "red"
; Interpretation: lights of a traffic light
; Examples:

(define TRAFFIC-LIGHT-GREEN "green")
(define TRAFFIC-LIGHT-YELLOW "yellow")
(define TRAFFIC-LIGHT-RED "red")

(define CIRCLE-RADIUS 25)

(define (traffic-light-temp tl)
  (cond
    [(string=? TRAFFIC-LIGHT-GREEN)...]
    [(string=? TRAFFIC-LIGHT-YELLOW)...]
    [(string=? TRAFFIC-LIGHT-RED)...]))
    

; main : World -> World
; A World is a TrafficLight
; Produces a cycling traffic light

(require 2htdp/universe)
(define (main initial-world)
  (big-bang initial-world
    [to-draw draw-traffic]
    [on-tick next-traffic 1]))

; draw-traffic : World -> Image
; draws a traffic signal

(check-expect (draw-traffic TRAFFIC-LIGHT-RED)
              (cond
                [(string=? tl TRAFFIC-LIGHT-RED) (circle CIRCLE-RADIUS "solid" TRAFFIC-LIGHT-RED)]))



; next-traffic : TrafficLight -> TrafficLight

                (check-expect (next-traffic TRAFFIC-LIGHT-RED) traffi

(define (traffic-light-temp tl)
  (cond
    [(string=? TRAFFIC-LIGHT-GREEN) TRAFFIC-LIGHT-YELLOW]
    [(string=? TRAFFIC-LIGHT-YELLOW)...]
    [(string=? TRAFFIC-LIGHT-RED)...]))
                              \
