;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pset 7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;ex 1

(define-struct circl [radius mode c])
(define-struct squar [side-length mode c])
(define-struct rectangl [width height mode c])
 
; A Shape is one of:
; - (make-circl Number Mode String)
; - (make-squar Number Mode String)
; - (make-rectangl Number Number Mode String)
; Interpretation: One of multiple shapes
; - make-circl
;   - radius is the radius in pixels
;   - mode is the drawing mode
;   - c is the color to draw the circle
; - make-squar
;   - side-length is the side length in pixels
;   - mode is the drawing mode
;   - c is the color to draw the square
; - make-rectangl
;   - width is the width in pixels
;   - height is the height in pixels
;   - mode is the drawing mode
;   - c is the color to draw the square

(define SHAPE-CIRCL-1 (make-circl 25 "solid" "red"))
(define SHAPE-SQUAR-1 (make-squar 25 "solid" "red"))
(define SHAPE-RECTANGL-1 (make-rectangl 50 25 "solid" "red"))

#;
(define (shape-temp shape)
  (cond
    [(circl? shape) ...
     (circl-radius shape) ... (circl-mode shape) ... (circl-c shape)...]
    [(squar? shape) ...
     (squar-side-length shape) ... (squar-mode shape) ... (squar-c shape) ...]
    [(rectangl? shape) ...
     (rectangl-width shape) ... (rectangl-height shape) ... (rectangl-mode shape) ...
     (rectangl-c shape) ...]))

; A Mode is one of:
; - "solid"
; - "outline"
; Interpretation: The drawing mode for a shape (either filled or outline)

(define MODE-S "solid")
(define MODE-O "outline")

#;
(define (mode-temp m)
  (cond
    [(string=? m "solid") ...]
    [(string=? m "outline") ...]))

;ex 2

; circle is a function that draws a circle, circl is a data structure with fields [radius mode c]

;ex 3

; draw-shape: Shape -> Image
; draws an shape
(check-expect (draw-shape SHAPE-CIRCL-1) (circle 25 "solid" "red"))
(check-expect (draw-shape SHAPE-SQUAR-1) (square 25 "solid" "red"))
(check-expect (draw-shape SHAPE-RECTANGL-1) (rectangle 50 25 "solid" "red"))

(define (draw-shape shape)
  (cond
    [(circl? shape) (circle 
                     (circl-radius shape) (circl-mode shape) (circl-c shape) )]
    [(squar? shape) (square
                     (squar-side-length shape) (squar-mode shape) (squar-c shape))]
    [(rectangl? shape) (rectangle
                        (rectangl-width shape) (rectangl-height shape)
                        (rectangl-mode shape) (rectangl-c shape))]))

;ex 4

; mode-swap: Shape -> Shape
; switches the mode of a Shape
(check-expect (mode-swap SHAPE-CIRCL-1) (make-circl 25 "outline" "red"))
(check-expect (mode-swap SHAPE-SQUAR-1) (make-squar 25 "outline" "red"))
(check-expect (mode-swap (make-squar 25 "outline" "red")) (make-squar 25 "solid" "red"))
(check-expect (mode-swap SHAPE-RECTANGL-1) (make-rectangl 50 25 "outline" "red"))
(check-expect (mode-swap (make-circl 26 "outline" "red")) (make-circl 26 "solid" "red"))

(define (mode-swap sh)
  (cond
    [(circl? sh) (if
                  (string=? (circl-mode sh) "outline")
                  (make-circl (circl-radius sh) "solid" (circl-c sh))
                  (make-circl (circl-radius sh) "outline" (circl-c sh)))]
    [(squar? sh) (if
                  (string=? (squar-mode sh) "outline")
                  (make-squar (squar-side-length sh) "solid" (squar-c sh))
                  (make-squar (squar-side-length sh) "outline" (squar-c sh)))]
    [(rectangl? sh) (if
                     (string=? (rectangl-mode sh) "outline")
                     (make-rectangl
                      (rectangl-width sh) (rectangl-height sh) "solid" (rectangl-c sh))
                     (make-rectangl
                      (rectangl-width sh) (rectangl-height sh) "outline" (rectangl-c sh)))]))


;ex 5

(define-struct monkey [name c others])
 
; A MonkeyChain is one of:
; - "barrel"
; - (make-monkey String String MonkeyChain)

; Interpretation: A collection of monkeys
; - "barrel" is an empty barrel
; - make-monkey
;   - name is the name of this monkey
;   - c is the color of this monkey
;   - others is the other monkeys (or barrel) it is attached to

(define CHAIN-0 "barrel")
(define CHAIN-1 (make-monkey "George" "red" CHAIN-0))
(define CHAIN-2 (make-monkey "Jefe" "purple" CHAIN-1))
(define CHAIN-3 (make-monkey "Dasani" "blue" CHAIN-2))

#;
(define (monkey-temp mon)
  (cond
    [(string? mon) CHAIN-0]
    [(monkey? mon) ...
     (monkey-name mon) ... (monkey-c mon) ... (monkey-temp (monkey-others mon)) ...]))

;ex 6
; purple-count: MonkeyChain -> Number
; counts the number of purple monkeys in a MonkeyChain

(check-expect (purple-count CHAIN-1) 0)
(check-expect (purple-count CHAIN-2) 1)
(check-expect (purple-count CHAIN-0) 0)
(check-expect (purple-count (make-monkey "Esteban" "purple" CHAIN-2)) 2)

(define (purple-count mc)
  (cond
    [(string? mc) 0]
    [(monkey? mc)
     (if
      (string=? (monkey-c mc) "purple")
      (add1 (purple-count (monkey-others mc)))
      (purple-count (monkey-others mc)))]))

;ex 7
; name-check: MonkeyChain String -> Boolean
; determines if a monkey with that name is in a MonkeyChain
(check-expect (name-check CHAIN-2 "Jefe") #true)
(check-expect (name-check CHAIN-2 "George") #true)
(check-expect (name-check CHAIN-2 "Steve") #false)
(check-expect (name-check CHAIN-0 "Jefe") #false)

(define (name-check mc name)
  (cond
    [(string? mc) #false]
    [(monkey? mc)
     (if 
      (string=? (monkey-name mc) name)
      #true
      (name-check (monkey-others mc) name))]))

;ex 8
(define TEXT-SIZE 24)

; name-stack: MonkeyChain -> Image
; displays colored MonkeyChain with a descriptor

(check-expect (name-stack CHAIN-0) (above
                                    (text "The monkey names are:" TEXT-SIZE "violet")
                                    empty-image))
(check-expect (name-stack CHAIN-2) (above
                                    (text "The monkey names are:" TEXT-SIZE "violet")
                                    (text "Jefe" TEXT-SIZE "purple")
                                    (text "George" TEXT-SIZE "red")
                                    empty-image))

(define (name-stack mc)
  (above (text "The monkey names are:" TEXT-SIZE "violet") (lister mc)))

; lister: MonkeyChain -> Image
; displays colord names of a MonkeyChain in order one above another

(check-expect (lister CHAIN-0) empty-image)
(check-expect (lister CHAIN-2) (above
                                (text "Jefe" TEXT-SIZE "purple")
                                (text "George" TEXT-SIZE "red")
                                empty-image))
(check-expect (lister CHAIN-1) (above
                                (text "George" TEXT-SIZE "red")
                                empty-image))

(define (lister mc)
  (cond
    [(string? mc)
     empty-image]
    [(monkey? mc)
     (above (text (monkey-name mc) TEXT-SIZE (monkey-c mc)) (lister (monkey-others mc)))]))

;ex 9
; A ListofBooleans (LoB) is one of:
; - '()
; (cons Boolean LoN)

(define LOB-0 '())
(define LOB-1 (cons #true (cons #false '())))
(define LOB-2 (cons #false (cons #false '())))

#;
(define (lob-temp lob)
  (cond
    [(empty? lob) ...]
    [(cons? lob) ...
     (first lob) ... (lob-temp (rest lob))]))

;ex 10
; contains-true: LoB -> Boolean
; sorts through a list and returns true if there is a #true value in it
(check-expect (contains-true LOB-0) #false)
(check-expect (contains-true LOB-1) #true)
(check-expect (contains-true LOB-2) #false)

(define (contains-true lob)
  (cond
    [(empty? lob) #false]
    [(cons? lob) (or
                  (first lob)
                  (contains-true (rest lob)))]))

;ex 11
; negator: LoB -> LoB
; negates all elements in a LoB
(check-expect (negator LOB-0) LOB-0)
(check-expect (negator LOB-1) (cons #false (cons #true '())))
(check-expect (negator LOB-2) (cons #true (cons #true '())))

(define (negator lob)
  (cond
    [(empty? lob) LOB-0]
    [(cons? lob) (if
                  (first lob)
                  (cons #false (negator (rest lob)))
                  (cons #true (negator (rest lob))))]))

                                    





