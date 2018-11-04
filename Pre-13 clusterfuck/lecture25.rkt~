;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture25) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Yesterday we talked about taking in two complex inputs. Today we're going to deal with mutually
;; recursive data definitions (think of our Webpage definition from lecture 23)

;; A while back (say, 2001) webpages were made of just HTML, without all the fancy Javascript and
;; such that exists today. Let's try to design a data definition that represents HTML (or at
;; least some fragment of it).

;; To see an example of HTML go to your favorite webpage and right click (or the Mac equivalent.
;; Do Macs have a right click yet? I don't know) and click "View page source". You should see the
;; HTML that makes up that webpage. Notice that HTML has a nested structure, that is: tags can go
;; inside of other tags.

(require 2htdp/image)

;; A Div is one of:
;; - String
;; - Image
;; - OL
;; - UL
;; and represents a piece of HTML on a webpage

(define-struct ol [content])
;; An OL is a (make-ol [List-of Div])
;; and represents an ordered list of HTML elements

(define-struct ul [content])
;; A UL is a (make-ul [List-of Div])
;; and represents an unordered list of HTML elements

(define DIV-STR1 "hello world")
(define DIV-STR2 "goodbye cruel world")
(define DIV-IMG1 (circle 10 "solid" "red"))
(define DIV-IMG2 (square 7 "outline" "blue"))
(define DIV-OL1 (make-ol '()))
(define DIV-OL2 (make-ol (list DIV-STR1 DIV-IMG1)))
(define DIV-UL1 (make-ul '()))
(define DIV-UL2 (make-ul (list DIV-STR2 DIV-IMG2)))
(define DIV-UL3 (make-ul (list DIV-STR2 DIV-IMG2 DIV-OL2)))

;; div-temp : Div -> ???
(define (div-temp d)
  (cond [(string? d) ...]
        [(image? d) ...]
        [(ol? d) (ol-template d)]
        [(ul? d) (ul-template d)]))

;; ol-template : OL -> ???
(define (ol-template ol)
  (... (lod-temp (ol-content ol)) ...))

;; lod-temp : [List-of Div] -> ???
(define (lod-temp lod)
  (cond [(empty? lod) ...]
        [(cons? lod) (... (div-temp (first lod)) ...
                          (lod-temp (rest lod)) ...)]))

;; ul-template : UL -> ???
(define (ul-template ul)
  (... (lod-temp (ul-content ul)) ...))

;; See how the structure of our code follows the structure of our data? We have just done the same
;; thing we always did. The only difference is that we have templates that refer to each other.

;; Let's design the function num-divs that accepts a Div and returns the total number of Divs inside
;; of it.

;; num-divs : Div -> Nat
;; Produces the total number of Divs inside the given Div
(check-expect (num-divs DIV-STR1) 1)
(check-expect (num-divs DIV-IMG1) 1)
(check-expect (num-divs DIV-OL2) 3)
(check-expect (num-divs DIV-UL3) 6)
(define (num-divs d)
  (cond [(string? d) 1]
        [(image? d) 1]
        [(ol? d) (num-divs/ol d)]
        [(ul? d) (num-divs/ul d)]))

;; num-divs/ol : OL -> Nat
;; Produces the total number of Divs inside this ordered list
(check-expect (num-divs/ol DIV-OL1) 1)
(check-expect (num-divs/ol DIV-OL2) 3)
(define (num-divs/ol ol)
  (+ 1 (num-divs/lod (ol-content ol))))

;; num-divs/lod : [List-of Div] -> Nat
;; Counts the number of Divs in the list
(check-expect (num-divs/lod '()) 0)
(check-expect (num-divs/lod (list DIV-STR1 DIV-OL2 DIV-IMG2)) 5)
(define (num-divs/lod lod)
  (local [;; Div Nat -> Nat
          ;; Add the number of divs in the given div to the total sofar
          (define (add-div d sofar)
            (+ (num-divs d) sofar))]
    (foldr add-div 0 lod)))

;; num-divs/ul : UL -> Nat
;; Produces the total number of Divs inside this unordered list
(check-expect (num-divs/ul DIV-UL1) 1)
(check-expect (num-divs/ul DIV-UL3) 6)
(define (num-divs/ul ul)
  (+ 1 (num-divs/lod (ul-content ul))))

;; BECCA: Just something to note. In the last few lectures we have had data definitions containing
;; lists. We write the template as if we were going to go through that list structurally (that is,
;; using our old list template instead of an abstraction) but we almost always ignore that template
;; and just end up with a list abstraction. So why don't we just use the list abstraction template
;; from the beginning? Well, the structural template with the conditional and everything makes it
;; MUCH clearer how the data in the list connects to our other data definitions. So that is the
;; preferred template even though we mostly end up ignoring it. The structural template also
;; helps us know what kind of a helper function we might need (as in, we definitely need to
;; individually process a single Div in the list).

;; Let's design render-div that accepts a Div and returns the image of that Div as it would appear
;; in a web browser.

(define TEXT-SIZE 20)
(define TEXT-COLOR "black")
(define BULLET (circle 3 "solid" "black"))

(define RENDER-DIV-STR1 (text "hello world" TEXT-SIZE TEXT-COLOR))
(define RENDER-DIV-STR2 (text "goodbye cruel world" TEXT-SIZE TEXT-COLOR))
(define RENDER-DIV-IMG1 DIV-IMG1)
(define RENDER-DIV-IMG2 DIV-IMG2)
(define RENDER-DIV-OL2
  (above/align "left"
               (beside (text "1. " TEXT-SIZE TEXT-COLOR) RENDER-DIV-STR1)
               (beside (text "2. " TEXT-SIZE TEXT-COLOR) RENDER-DIV-IMG1)))
(define RENDER-DIV-UL2
  (above/align "left"
               (beside BULLET RENDER-DIV-STR2)
               (beside BULLET RENDER-DIV-IMG2)))
(define RENDER-DIV-UL3
  (above/align "left"
               (beside BULLET RENDER-DIV-STR2)
               (beside BULLET RENDER-DIV-IMG2)
               (beside BULLET RENDER-DIV-OL2)))

;; render-div : Div -> Image
;; Render the given Div as an image
(check-expect (render-div DIV-STR1) RENDER-DIV-STR1)
(check-expect (render-div DIV-IMG1) RENDER-DIV-IMG1)
(check-expect (render-div DIV-OL2) RENDER-DIV-OL2)
(check-expect (render-div DIV-UL2) RENDER-DIV-UL2)
(define (render-div d)
  (cond [(string? d) (text d TEXT-SIZE TEXT-COLOR)]
        [(image? d) d]
        [(ol? d) (render-ol d)]
        [(ul? d) (render-ul d)]))

;; render-ol : OL -> Image
;; Render the ordered list as an image
(check-expect (render-ol DIV-OL1) empty-image)
(check-expect (render-ol DIV-OL2) RENDER-DIV-OL2)
(define (render-ol ol)
  (render-lod/ordered (ol-content ol) 1))

;; render-lod/ordered : [List-of Div] Nat -> Image
(check-expect (render-lod/ordered '() 10) empty-image)
(check-expect (render-lod/ordered (list DIV-STR1 DIV-IMG1) 1) RENDER-DIV-OL2)
(define (render-lod/ordered lod n)
  (cond [(empty? lod) empty-image]
        [(cons? lod)
         (above/align "left"
                      (beside (number-text n) (render-div (first lod)))
                      (render-lod/ordered (rest lod) (add1 n)))]))

;; number-text : Nat -> Image
;; Produce an image of this number with a ". " after it
(check-expect (number-text 2) (text "2. " TEXT-SIZE TEXT-COLOR))
(check-expect (number-text 10) (text "10. " TEXT-SIZE TEXT-COLOR))
(define (number-text n)
  (text (string-append (number->string n) ". ") TEXT-SIZE TEXT-COLOR))

;; render-ul : UL -> Image
;; Render the unordered list as an image
(check-expect (render-ul DIV-UL1) empty-image)
(check-expect (render-ul DIV-UL2) RENDER-DIV-UL2)
(define (render-ul ul)
  (render-lod/unordered (ul-content ul)))

;; render-lod/unordered : [List-of Div] -> Image
;; Render the unordered list as an image with bullets beside each item
(check-expect (render-lod/unordered '()) empty-image)
(check-expect (render-lod/unordered (list DIV-STR2 DIV-IMG2)) RENDER-DIV-UL2)
(define (render-lod/unordered lod)
  (local [;; Div Image -> Image
          ;; Add the image of the given div (with a bullet next to it) above the image
          (define (add-div-render d sofar)
            (above/align "left" (beside BULLET (render-div d)) sofar))]
    (foldr add-div-render empty-image lod)))

;; The render-lod/ordered function is a bit more complicated because we need an extra input
;; to tell the function where the numbering should start. Notice that I didn't use a list
;; abstraction here. That's because we're actually recurring over TWO things: the list and
;; the number. So we can't use a list abstraction which ONLY recurs over a list.