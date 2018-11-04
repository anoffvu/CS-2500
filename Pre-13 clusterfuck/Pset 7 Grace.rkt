;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pset 7 Grace|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct circl [radius mode c])
(define-struct squar [side-length mode c])
(define-struct rectangl [width height mode c])
 
; A Shape is one of:
; - (make-circl Number Mode String)
; - (make-squar Number Mode String)
; - (make-rectangl Number Number Mode String)
; 
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

; Ex 1
(define CIRCL-1 (make-circl 20 "outline" "red"))
(define CIRCL-2 (make-circl 20 "solid" "red"))
(define SQUAR-1 (make-squar 30 "outline" "blue"))
(define RECTANGL-1 (make-rectangl 30 40 "solid" "green"))
;Template
#;
(define (shape-temp s)
  (cond
    [(circl? s) ... (circl-radius s)
                ... (mode-temp (circl-mode s))
                ... (circl-c s)...]
    [(squar? s) ... (squar-side-length s)
                ... (mode-temp (squar-mode s))
                ... (squar-c s)...]
    [(rectangl? s) ... (rectangl-width s)
                   ... (rectangl-height s)
                   ... (mode-temp (rectangl-mode s))
                   ... (rectangl-c s)...]))
 
; A Mode is one of:
; - "solid"
; - "outline"
; Interpretation: The drawing mode for a shape (either filled or outline)
(define SOLID "solid")
(define OUTLINE "outline")
#;
(define (mode-temp m)
  (cond
    [(string=? m SOLID) ... m ...]
    [(string=? m OUTLINE) ... m ...]))

; Ex 2
; A circle is a function that takes in a Number, and 2 Strings and outputs an image of a circle
; A circl is a structure that has 3 drawers

; Ex 3
; shape-image : Shape -> Image
; Renders the image of the given Shape
(check-expect (shape-image CIRCL-1) (circle 20 "outline" "red"))
(check-expect (shape-image CIRCL-2) (circle 20 "solid" "red"))
(check-expect (shape-image SQUAR-1) (square 30 "outline" "blue"))
(check-expect (shape-image RECTANGL-1) (rectangle 30 40 "solid" "green"))
(define (shape-image s)
  (cond
    [(circl? s)
     (circle (circl-radius s)
             (circl-mode s)
             (circl-c s))]
    [(squar? s)
     (square (squar-side-length s)
             (squar-mode s)
             (squar-c s))]
    [(rectangl? s)
     (rectangle (rectangl-width s)
                (rectangl-height s)
                (rectangl-mode s)
                (rectangl-c s))]))

; Ex 4
; flip-mode : Shape -> Shape
; Takes in a shape and flips its mode
(check-expect (flip-mode CIRCL-1) CIRCL-2)
(check-expect (flip-mode SQUAR-1) (make-squar 30 "solid" "blue"))
(check-expect (flip-mode RECTANGL-1) (make-rectangl 30 40 "outline" "green"))
(define (flip-mode s)
  (cond
    [(circl? s) (make-circl (circl-radius s)
                            (flip (circl-mode s))
                            (circl-c s))]
    [(squar? s) (make-squar (squar-side-length s)
                            (flip (squar-mode s))
                            (squar-c s))]
    [(rectangl? s) (make-rectangl (rectangl-width s)
                                  (rectangl-height s)
                                  (flip (rectangl-mode s))
                                  (rectangl-c s))]))

; flip : Mode -> Mode
; Takes in a mode and outputs the other mode
(check-expect (flip SOLID) OUTLINE)
(check-expect (flip OUTLINE) SOLID)
(define (flip m)
  (cond
    [(string=? m SOLID) OUTLINE]
    [(string=? m OUTLINE) SOLID]))

(define-struct monkey [name c others])
 
; A MonkeyChain is one of:
; - "barrel"
; - (make-monkey String String MonkeyChain)
; 
; Interpretation: A collection of monkeys
; - "barrel" is an empty barrel
; - make-monkey
;   - name is the name of this monkey
;   - c is the color of this monkey
;   - others is the other monkeys (or barrel) it is attached to

; Ex 5
(define MC-0 "barrel")
(define MC-1 (make-monkey "grace" "pink" MC-0))
(define MC-2 (make-monkey "kate" "purple" MC-1))
(define MC-3 (make-monkey "cat" "blue" MC-2))
#;
(define (mc-temp mc)
  (cond
    [(string? mc) ... mc ...]
    [(monkey? mc) ... (monkey-name mc)
                  ... (monkey-c mc)
                  ... (mc-temp (monkey-others mc))...]))

; Ex 6
; num-of-purple : MonkeyChain -> PositiveInteger
; Determines how many purple monkeys are in a MonkeyChain
(check-expect (num-of-purple MC-0) 0)
(check-expect (num-of-purple MC-1) 0)
(check-expect (num-of-purple MC-2) 1)
(check-expect (num-of-purple (make-monkey "foo" "purple" MC-3)) 2)
(define (num-of-purple mc)
  (cond
    [(string? mc) 0]
    [(monkey? mc) (if (string=? (monkey-c mc) "purple")
                      (+ (num-of-purple (monkey-others mc)) 1)
                      (num-of-purple (monkey-others mc)))]))

; Ex 7
; monkey-in-chain? : MonkeyChain String -> Boolean
; determines if the given name is the name of a monkey in the given MonkeyChain
(check-expect (monkey-in-chain? MC-0 "grace") #false)
(check-expect (monkey-in-chain? MC-1 "grace") #true)
(check-expect (monkey-in-chain? MC-3 "cat") #true)
(check-expect (monkey-in-chain? MC-3 "grace") #true)
(define (monkey-in-chain? mc name)
  (cond
    [(string? mc) #false]
    [(monkey? mc) (or (string=? name (monkey-name mc))
                      (monkey-in-chain? (monkey-others mc) name))]))

; Ex 8
(define TEXT-SIZE 24)
; create-chain : MonkeyChain -> Image
; Takes a MonkeyChain and creates an image of the MonkeyChain's colored monkey names
; stacked on top of each other with a title on top
(check-expect (create-chain MC-0) (above (text "The monkey names are:" TEXT-SIZE "violet")
                                         empty-image))
(check-expect (create-chain MC-1) (above (text "The monkey names are:" TEXT-SIZE "violet")
                                         (text "grace" TEXT-SIZE "pink")
                                         empty-image))
(check-expect (create-chain MC-2) (above (text "The monkey names are:" TEXT-SIZE "violet")
                                         (text "kate" TEXT-SIZE "purple")
                                         (text "grace" TEXT-SIZE "pink")
                                         empty-image))
(define (create-chain mc)
  (above (text "The monkey names are:" TEXT-SIZE "violet") (monkey-list mc)))

; monkey-list : MonkeyChain -> Image
; Creates an image of the stacked, colored monkey names in the right order
(check-expect (monkey-list MC-0) empty-image)
(check-expect (monkey-list MC-1) (above (text "grace" TEXT-SIZE "pink")
                                        empty-image))
(check-expect (monkey-list MC-2) (above (text "kate" TEXT-SIZE "purple")
                                        (text "grace" TEXT-SIZE "pink")
                                        empty-image))
(define (monkey-list mc)
  (cond
    [(string? mc) empty-image]
    [(monkey? mc) (above
                   (text (monkey-name mc) TEXT-SIZE (monkey-c mc))
                   (monkey-list (monkey-others mc)))]))

; Ex 9
; A ListofBooleans (LoB) is one of:
; - '()
; - (cons Boolean LoB)
(define LOB-1 '())
(define LOB-2 (cons #true LOB-1))
(define LOB-3 (cons #true LOB-2))
(define LOB-4 (cons #false LOB-3))
#;
(define (lob-temp lob)
  (cond
    [(empty? lob) ... lob ...]
    [(cons? lob) ... (first lob) ... (lob-temp (rest lob)) ...]))

; Ex 10
; true? : LoB -> Boolean
; Determines whether any element in the LoB is #true
(check-expect (true? LOB-1) #false)
(check-expect (true? LOB-2) #true)
(check-expect (true? LOB-3) #true)
(check-expect (true? (cons #false LOB-1)) #false)
(define (true? lob)
  (cond
    [(empty? lob) #false]
    [(cons? lob) (or (first lob) (true? (rest lob)))]))

; Ex 11
; swapper : LoB -> LoB
; Reverses each boolean in the LoB
(check-expect (swapper LOB-1) LOB-1)
(check-expect (swapper LOB-2) (cons #false LOB-1))
(check-expect (swapper LOB-4) (cons #true (cons #false (cons #false LOB-1))))
(define (swapper lob)
  (cond
    [(empty? lob) lob]
    [(cons? lob) (cons (not (first lob)) (swapper (rest lob)))]))
  
  
                                         
             
  
