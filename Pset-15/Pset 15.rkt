;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Pset 15|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;ex 1

; An Ending is a (make-ending String Boolean)
(define-struct ending [text good?])
; and represents an ending to the story with some text and whether it was a happy ever after

#;
(define (ending-temp e)
  ... (ending-text e) ... (ending-good? e) ...)

; An [NEList-of X] (Non-Empty List) is one of:
; - (cons X '())
; - (cons X [NEList-of X])

; A Choice is a a (make-choice String Section)
(define-struct choice [text result])
; and represents the blurb shown to the reader and the resulting section if that is the
; choice they make
(define CHOICE-1 (make-choice "bacon" ENDING-1))
(define CHOICE-2 (make-choice "pure lard" ENDING-2))
(define CHOICE-3 (make-choice "candles" ENDING-3))
(define CHOICE-7 (make-choice "home" ENDING-4))


; A Chapter is a (make-chapter String [NEList-of Choice])
(define-struct chapter [text choices])
; and represents a chapter in a choose-your-own adventure book with a body
; and a list of choices at the end
(define CHAPTER-3 (make-chapter "Now I feel fat, I should go" (list CHOICE-7)))
(define CHAPTER-2 (make-chapter "I just ate" (list CHOICE-1 CHOICE-2 CHOICE-3)))

#;
(define (chapter-temp c)
  ... (chapter-text c) ... (chapter-choices c) ...)

(define CHOICE-4 (make-choice "yes" CHAPTER-2))
(define CHOICE-5 (make-choice "yes" CHAPTER-2))
(define CHOICE-6 (make-choice "yes" CHAPTER-2))
(define CHAPTER-1 (make-chapter "You just got to the buffet. Do you eat?"
                                 (list CHOICE-4 CHOICE-5 CHOICE-6)))


; A Section is one of:
; - Ending
; - Chapter
; and represents a section in a choose your own adventure book
(define SECTION-1 (make-ending "End 1" #true))
(define SECTION-2 ENDING-1)
(define SECTION-3 CHAPTER-1)

#;
(define (section-temp s)
  (cond
    [(ending? s) ... (ending-temp s) ...]
    [(chapter? s) ... (chapter-temp s) ...]))
 
(define MY-STORY
  (make-chapter
   "You are alone in a room. There is a door before you."
   (list
    (make-choice
     "Stay in the room."
     (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f))
    (make-choice
     "Open the door."
     (make-chapter
      "You open the door. On the other side lays an eternity of nothingness."
      (list
       (make-choice
        "Scream."
        (make-ending
         "You scream into the void. There is no response. There never will be." #f))
       (make-choice
        "Accept your fate."
        (make-ending
         "You accept the simple beauty of the void. You achieve nirvana." #t))))))))

;ex 2
; possible-endings : Section -> Number
; calculates the total possible number of endings of a story