;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname notes) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; LECTURE 1 (September 5, 2018)

;; The course website is: https://course.ccs.neu.edu/cs2500/
;; Please read the website! It has a lot of useful information about the course!

;; There are two parts of DrRacket: the top area (the definitions area) and the bottom area
;; (the interactions area). When writing your program put it in the DEFINITIONS area. When
;; you are using your program (interacting with it) you can use the INTERACTIONS area. Please
;; note that you CANNOT save anything that you do in the interactions area. So anything you want
;; to save you will need top ut in the definitions area.

;; NUMBERS
(+ 3 9) ;; 3+9
(/ (sqr 4) 2) ;; (4^2)/2
(        *         2                            3 ) ;; 2*3
;; Note that spaces don't really matter, apart from separating operators and arguments
(* 3.25 7.4) ;; 3.25*7.4 (decimal numbers are also allowed!)
pi ;; pi = 3.14159.....
;; This is an irrational number so you will see a #i at the beginning of the number to
;; indicate that this is only an APPROXIMATION of pi, since the computer cannot store
;; all of the infinite digits of pi
(sqrt 2) ;; this will also produce an irrational number
(sqrt -9) ;;0+3i (this is an imaginary number which is what the i means here)
(/ 1 3)
;; This produces a repeating decimal, so you will see a line above the .3 to indicate that
;; the digit goes on forever

;; STRINGS !
"hello"
"" ;; this is called the empty string
"\"hello\"" ;; the string hello with quotes!
;; The backslash ESCAPES the string (it tells BSL not to treat the quotes as usual)
"\\" ;; the string with the text \ (again, we escape with \)

;; IMAGES
(require 2htdp/image) ;; YOU MUST USE THIS TO GET IMAGES
(circle 40 "solid" "red") ;; a solid red circle with a radius of 40 pixels
(circle 10 "solid" "some nonsense here")
;; the above will give you a black circle because it doesn't know what color you mean
(beside (circle 20 "solid" "blue") (circle 30 "solid" "orange"))
;; beside is an arithmetic function (like +) that works on IMAGES rather than on NUMBERS
(rectangle 2 4 "outline" "purple")

;; DOCUMENTATION
;; You can right click any function (e.g. circle, sqrt) and click the "Search in Help Desk" option
;; to see the documentation for that function.

;; You can see a list of all the colors you can use when drawing images here:
;; http://docs.racket-lang.org/draw/color-database___.html

;; TO DO
;; Get a CCIS account at my.ccs.neu.edu by tomorrow's lecture. If you have trouble email me
;; (Rebecca MacKenzie) at r.mackenzie@northeastern.edu

;; COURSE INFO
;; Homeworks are due on Mondays and Thursdays at 9pm. The first one is not due until the 13th.
;; We will discuss how to submit these homeworks in lab on Tuesday.