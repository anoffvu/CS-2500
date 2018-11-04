;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture19) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Let's continue with our wheel of fortune game from the previous lecture

;; DATA DEFINITIONS

(define-struct guessed [char])
(define-struct unguessed [char])

;; A Game1String is one of:
;; - (make-guessed 1String)
;;   - where char is the letter that has been guessed and revealed
;; - (make-unguessed 1String)
;;   - where char is the letter the player needs to guess

(define GUESSED1 (make-guessed "a"))
(define GUESSED2 (make-guessed "p"))
(define UNGUESSED1 (make-unguessed "a"))
(define UNGUESSED2 (make-unguessed "p"))

;; game1string-temp : Game1String -> ???
(define (game1string-temp g1s)
  (cond [(guessed? g1s) (... (guessed-char g1s) ...)]
        [(unguessed? g1s) (... (unguessed-char g1s) ...)]))

;; A WheelOfFortune (WOF) is a [List-of Game1String]

(define WOF0 '())
(define WOF1 (list UNGUESSED1 UNGUESSED2))
(define WOF2 (list UNGUESSED1 GUESSED2))
(define WOF3 (list GUESSED1 UNGUESSED2))
(define WOF4 (list GUESSED1 GUESSED2))

;; wof-template : WOF -> ???
(define (wof-template wof)
  (cond [(empty? wof) ...]
        [(cons? wof)
         ; why is it cons here and not list?
         (... (game1string-temp (first wof))
              (wof-template (rest wof)) ...)]))

;; CONSTANTS
(define FONT-SIZE 20)
(define FONT-COLOR "black")
(define BOX-SIZE (* FONT-SIZE 2))
(define OUTLINE-BOX-IMAGE
  (overlay (square BOX-SIZE "outline" "black") (square (+ BOX-SIZE 4) "solid" "white")))
(define UNGUESSED-IMAGE
  (overlay (text "?" FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))

;; FUNCTIONALITY

;; guessing-game : String -> WOF
;; Play wheel of fortune with the given word
(define (guessing-game word)
  (big-bang (initial-world-state word)
    [to-draw draw-wof]
    [on-key guess-letter]
    [stop-when all-letters-revealed? draw-wof]))

;; initial-world-state : String -> WOF
;; Produces a WOF for the given string where all letters are unguessed
(check-expect (initial-world-state "") '())
(check-expect (initial-world-state "ap") (list UNGUESSED1 UNGUESSED2))
(define (initial-world-state s)
  (map make-unguessed (explode s)))

;; BECCA: I changed the drawing function just a tad because I wanted to do everything in one
;; iteration as opposed to first converting the list to a list of strings and then appending
;; them together and then drawing them. You can also do it the way Professor Mislove did
;; and it is just as valid. I just think this way looks better when you're playing the game :)

;; draw-wof : WOF -> Image
;; Draw the given wheel of fortune game
(check-expect (draw-wof WOF0) empty-image)
(check-expect (draw-wof WOF2) (draw-beside UNGUESSED1 (draw-beside GUESSED2 empty-image)))
(define (draw-wof wof)
  (foldr draw-beside empty-image wof))

;; draw-beside : Game1String Image -> Image
;; Draw the given Game1String beside the given image
(check-expect
 (draw-beside GUESSED1 empty-image)
 (overlay (text "a" FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))
(check-expect
 (draw-beside UNGUESSED2 (square 10 "solid" "red"))
 (beside UNGUESSED-IMAGE (square 10 "solid" "red")))
(define (draw-beside g1s img)
  (cond [(guessed? g1s)
         (beside (draw-guessed (guessed-char g1s)) img)]
        [(unguessed? g1s)
         (beside UNGUESSED-IMAGE img)]))

;; draw-guessed : 1String -> Image
;; Draw a guessed letter
(check-expect
 (draw-guessed "x")
 (overlay (text "x" FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))
(check-expect
 (draw-guessed "a")
 (overlay (text "a" FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))
(define (draw-guessed letter)
  (overlay (text letter FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))

;; guess-letter : WOF KeyEvent -> WOF
;; Reveal all the letters that match the given key
(check-expect (guess-letter WOF0 "x") '())
(check-expect (guess-letter WOF2 "a") WOF4)
(define (guess-letter wof ke)
  (cond [(empty? wof) '()]
        [(cons? wof) (cons (reveal-if-match (first wof) ke)
                           (guess-letter (rest wof) ke))]))

;; Why can't we use map above? We are doing the same thing to every
;; element in the list, so it seems like the perfect opportunity. However,
;; notice that our helper (reveal-if-match) takes TWO arguments: the Game1String
;; AND the key that was pressed. The signature for map says that it takes a
;; function of 1 argument and we can't give it one. We'll find a way to work
;; around this later on but for now we need to return to our list template.

;; reveal-if-match : Game1String KeyEvent -> Game1String
;; If the letter matches the given key, reveal it, otherwise leave it the same
(check-expect (reveal-if-match UNGUESSED1 "a") GUESSED1)
(check-expect (reveal-if-match UNGUESSED2 "b") UNGUESSED2)
(check-expect (reveal-if-match GUESSED2 "x") GUESSED2)

(define (reveal-if-match g1s ke)
  (cond [(guessed? g1s) g1s]
        [(unguessed? g1s)
         (if 
          (string=? (unguessed-char g1s) ke)
          (make-guessed (unguessed-char g1s))
          g1s)]))


;; all-letters-revealed? : WOF -> Boolean
;; Are all the letters in the word revealed?
(check-expect (all-letters-revealed? WOF0) #true)
(check-expect (all-letters-revealed? WOF2) #false)
(check-expect (all-letters-revealed? WOF4) #true)
(define (all-letters-revealed? wof)
  (andmap guessed? wof))

;; Okay let's talk more about the problem we had when implementing our key handler.
;; It's super frustrating that we know all these useful abstractions but we couldn't
;; use any of them because we needed this little piece of context (the key event to
;; compare the string to). Let's talk about how to resolve this. To do this we
;; need to talk about something called SCOPE.

;; Recall from algebra: f(x) = x + 7
;; What is x? It's some number that we are inputting to the function. Where does it
;; exist? Where is it valid? It ONLY exists in the body of the function f. So for
;; example if I later wrote g(y) = y + x that wouldn't make any sense because x is
;; OUT OF SCOPE there. It isn't defined and it doesn't make sense to use it there.

;; Let's talk about this in terms of ISL.
(define (my-function x) (+ x 7))
;; In the same way, x only exists inside the body of my-function. Outside of that
;; DrRacket will give you some error to tell you that x isn't defined because it
;; really doesn't exist anywhere else. It's "out of scope".

;; So let's learn something new that introduces new scope. The new thing is called
;; 'local' and it introduces a new scope. You can think of it as a mini definitions window
;; basically.

;; Here's the syntax
#;(local [definition1
           definition2
           definition3]
    expression)

;; This evaluates to whatever the expression would evaluate to given the definitions in the
;; local. So here are some examples

(define EXAMPLE1 (local [(define x 5)] x)) ;; Evaluates to 5
(define EXAMPLE2 (local [(define (f x) (+ x 4))] (f 10))) ;; Evaluates to 14
(define EXAMPLE3 (local [(define x 5)
                         (define (g n) (- (sqr n) 3))]
                   (g x))) ;; evaluates to 22 (5 squared - 3)

;; Here's a tricky one:

(define EXAMPLE4 (local [(define x 5)
                         (define y 10)]
                   (local [(define x 20)]
                     (+ x y))))


;; The above example evaluates to 30 because x is 20 in the INNER scope. Basically, x gets
;; overwritten by the inner local definition. The inner local can use the y from the outer
;; local because we are still within scope of the outer local.

;; Click the "Check Syntax" button in the upper righthand corner of DrRacket and if you hover
;; over the x you will see an arrow pointing to the inner definition. This button shows you
;; where things are defined and it is showing which definition is going to be used.

;; Okay, why is this useful? Why not just use our regular definitions window and avoid
;; all of these crazy new things? Well, it turns out we can use local inside of a function
;; to define things that are only relevant to that function. Here's a brief example.

;; Let's write a function that takes a list of numbers AND a number and adds the given number
;; to every element of the list

;; add-to-everyone : [List-of Number] Number -> [List-of Number]
;; Adds the given number to every element of the list
(check-expect (add-to-everyone '() 5) '())
(check-expect (add-to-everyone (list 1 2 3) 10) (list 11 12 13))
(define (add-to-everyone lon n)
  (local [;; add-to-one : Number -> Number
          ;; Add n to the given number
          (define (add-to-one x) (+ x n))]
    (map add-to-one lon)))

;; Look at that! We can use map even though our function relied on two things, the element AND
;; the given number! Would that be useful to us at all? Was there maybe a case where we wanted
;; to use map but our function relied on two things instead of just one? Does that sound familiar?

;; BECCA: Just a quick note that you CANNOT name the input to 'add-to-one' n because it will
;; overwrite the n that was an input to 'add-to-everyone'. You can try doing this and see
;; what will happen. Try clicking the check syntax button to see where each n comes from.

;; It feels a little wrong to be skipping the check-expects here. Haven't we always told you
;; to test your functions? Haven't we been harping on the design recipe for weeks now? It turns
;; out that check-expect won't work inside a local so we can't do it. What does that mean?
;; 1. It does NOT mean that you can just write all your functions in a local and never test
;;     anything.
;; 2. It means that any function you define in a local should be VERY simple. It should be
;;     like, one or two lines at most. It should be dead easy to evaluate the result of that
;;     function. This MAY mean that you need to call a helper which is NOT locally defined.

;; Okay, so let's try to redefine our key handler from wheel of fortune.

;; guess-letter.v2 : WOF KeyEvent -> WOF
;; Reveal all the letters that match the given key
(check-expect (guess-letter.v2 WOF0 "x") '())
(check-expect (guess-letter.v2 WOF2 "a") WOF4)
(define (guess-letter.v2 wof ke)
  (local [;; guess-single-letter : Game1String -> Game1String
          ;; Reveal the letter if it matches the key that was pressed
          (define (guess-single-letter g1s)
            (reveal-if-match g1s ke))]
    (map guess-single-letter wof)))

;; BECCA: Notice how in guess-single-letter I just call a helper with some extra inputs. That's
;; because I want to be able to test the actual functionality where I transform the Game1String.
;; That function is too complicated to be untested. It has a conditional with two branches and an if
;; statement inside one of the branches. Professor Mislove put this all in the local definition
;; but I'm pretty sure that was just to demonstrate the point of using a local. You should not do
;; complicated stuff in a local. You should test your functions. You should not do everything in a
;; local. I'm just going to say that a lot so you know without a doubt that you should not do it.