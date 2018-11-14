;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ps15-abhinav-steven) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A Section is one of:
; - Ending
; - Chapter
; and represents a section in a choose your own adventure book

; section-temp : Section -> ???
(define (section-temp s)
  (cond
    [(ending? s) (... (ending-text s) ... (ending-good? s) ...)]
    [(chapter? s) (... (chapter-text s) ... (chapter-choices s) ...)]))
 
; An Ending is a (make-ending String Boolean)
(define-struct ending [text good?])
; and represents an ending to the story with some text and whether it was a happy ever after
 
; A Chapter is a (make-chapter String [NEList-of Choice])
(define-struct chapter [text choices])
; and represents a chapter in a choose-your-own adventure book with a body
; and a list of choices at the end
 
; An [NEList-of X] (Non-Empty List) is one of:
; - (cons X '())
; - (cons X [NEList-of X])
 
; A Choice is a a (make-choice String Section)
(define-struct choice [text result])
; and represents the blurb shown to the reader and the resulting section if that is the
; choice they make
 
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

; count-endings : Section -> Number
; Counts the number of possible endings to a section
(check-expect (count-endings MY-STORY) 3)

(define (count-endings s)
  (cond
    [(ending? s) 1]
    [(chapter? s) (count-chapter-endings (chapter-choices s))]))

; count-chapter-endings : [List-of Choice] -> Number
; Counts the number of possible endings in a list of choices
(check-expect (count-chapter-endings (chapter-choices MY-STORY)) 3)

(define (count-chapter-endings loc)
  (cond
    [(empty? loc) 0]
    [else (if (ending? (choice-result (first loc)))
              (add1 (count-chapter-endings (rest loc)))
              (+ (count-chapter-endings (chapter-choices (choice-result (first loc))))
                 (count-chapter-endings (rest loc))))]))

(define BACKGROUND (empty-scene 1200 800))
(define TEXT-SIZE 24)
(define TEXT-SIZE-BIG 36)
(define TEXT-COLOR "black")
(define TEXT-COLOR-SELECTED "green")
(define TEXT-COLOR-GOOD "blue")
(define TEXT-COLOR-BAD "red")

(define-struct selection [choices index])
; A SelectionList is a (make-selection [List-of Choice] Nat)
; - where choices is a list of choices
; - and index is the index of the currently selected choice

; selection-temp : SelectionList -> ???
(define (selection-temp sl)
  (... (selection-choices sl) ... (selection-index sl) ...))

; loc->selection : [List-of Choice] -> SelectionList
; Converts a list of choices to a SelectionList
(check-expect (loc->selection (chapter-choices MY-STORY))
              (make-selection (chapter-choices MY-STORY) 0))

(define (loc->selection loc)
  (make-selection loc 0))

; handle-up : SelectionList -> SelectionList
; Handles the selection of the up key
(check-expect (handle-up (make-selection (chapter-choices MY-STORY) 0))
              (make-selection (chapter-choices MY-STORY) 0))
(check-expect (handle-up (make-selection (chapter-choices MY-STORY) 1))
              (make-selection (chapter-choices MY-STORY) 0))

(define (handle-up sl)
  (cond
    [(= (selection-index sl) 0) sl]
    [else (make-selection (selection-choices sl) (- (selection-index sl) 1))]))

; handle-up-ss : SelectedSection -> SelectedSection
; Handles up for a SelectedSection
(check-expect (handle-up-ss (chapter->sc MY-STORY)) (chapter->sc MY-STORY))

(define (handle-up-ss ss)
  (cond
    [(ending? ss) ss]
    [(sel-chapter? ss) (make-sel-chapter (sel-chapter-text ss) (handle-up (sel-chapter-sl ss)))]))

; handle-down : SelectionList -> SelectionList
; Handles the selection of the down key
(check-expect (handle-down (make-selection (chapter-choices MY-STORY) 0))
              (make-selection (chapter-choices MY-STORY) 1))
(check-expect (handle-down (make-selection (chapter-choices MY-STORY) 1))
              (make-selection (chapter-choices MY-STORY) 1))

(define (handle-down sl)
  (cond
    [(= (selection-index sl) (- (length (selection-choices sl)) 1)) sl]
    [else (make-selection (selection-choices sl) (add1 (selection-index sl)))]))

; handle-down-ss : SelectedSection -> SelectedSection
; Handles down for a SelectedSection
(check-expect (handle-down-ss (chapter->sc MY-STORY))
              (make-sel-chapter (chapter-text MY-STORY)
                                (make-selection (chapter-choices MY-STORY) 1)))

(define (handle-down-ss ss)
  (cond
    [(ending? ss) ss]
    [(sel-chapter? ss) (make-sel-chapter (sel-chapter-text ss) (handle-down (sel-chapter-sl ss)))]))


; handle-enter : SelectionList -> SelectedSection
; Handles selection by the enter key
(check-expect (handle-enter (make-selection (chapter-choices MY-STORY) 0))
              (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f))
(check-expect (handle-enter (make-selection (chapter-choices MY-STORY) 1))
              (chapter->sc
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
                   "You accept the simple beauty of the void. You achieve nirvana." #t))))))

(define (handle-enter sl)
  (cond
    [(ending? (choice-result (list-ref (selection-choices sl) (selection-index sl))))
     (choice-result (list-ref (selection-choices sl) (selection-index sl)))]
    [(chapter? (choice-result (list-ref (selection-choices sl) (selection-index sl))))
     (chapter->sc (choice-result (list-ref (selection-choices sl) (selection-index sl))))]))

; handle-enter-ss : SelectedSection -> SelectedSection
; Handles enter for a SelectedSection

(define (handle-enter-ss ss)
  (cond
    [(ending? ss) ss]
    [(sel-chapter? ss) (handle-enter (sel-chapter-sl ss))]))

; key-handler : SelectedSection KeyEvent -> SelectedSection
; Key handler for on key
(define (key-handler ss ke)
  (cond
    [(string=? ke "up") (handle-up-ss ss)]
    [(string=? ke "down") (handle-down-ss ss)]
    [(or (string=? ke "\r") (string=? ke "enter")) (handle-enter-ss ss)]))

(define-struct sel-chapter [text sl])
; A SelectedChapter is a (make-sel-chapter String SelectionList)
; - where text is the chapter text
; - and sl is a SelectionList of choices
(define MY-STORY-SELECTED (make-sel-chapter (chapter-text MY-STORY)
                                            (loc->selection (chapter-choices MY-STORY))))

; sel-chapter-temp : SelectedChapter -> ???
(define (sel-chapter-temp sc)
  (... (sel-chapter-text sc) ... (selection-temp (sel-chapter-sl sc)) ...))

; chapter->sc : Chapter -> SelectedChapter
; Converts a Chapter to a SelectedChapter
(check-expect (chapter->sc MY-STORY) MY-STORY-SELECTED)

(define (chapter->sc c)
  (make-sel-chapter (chapter-text c) (loc->selection (chapter-choices c))))

(define SL-STORY (loc->selection (chapter-choices MY-STORY)))
(define SL-STORY1 (handle-up SL-STORY))
(define SL-STORY2 (handle-down SL-STORY))

; draw-state : SelectedSection -> Image
; Draws the selection window for the user
(check-expect (draw-state (chapter->sc MY-STORY))
              (overlay
               (above (text (sel-chapter-text (chapter->sc MY-STORY))
                            TEXT-SIZE-BIG TEXT-COLOR)
                      (draw-choices (sel-chapter-sl (chapter->sc MY-STORY))
                                    (selection-index (sel-chapter-sl (chapter->sc MY-STORY)))))
               BACKGROUND))
(check-expect (draw-state
               (make-ending "You accept the simple beauty of the void. You achieve nirvana." #t))
              (overlay (text "You accept the simple beauty of the void. You achieve nirvana."
                             TEXT-SIZE-BIG TEXT-COLOR-GOOD)
                       BACKGROUND))
(check-expect (draw-state
               (make-ending "You scream into the void.There is no response. There never will be." #f))
              (overlay (text "You scream into the void.There is no response. There never will be."
                             TEXT-SIZE-BIG TEXT-COLOR-BAD)
                       BACKGROUND))

(define (draw-state ss)
  (cond
    [(ending? ss) (if (ending-good? ss)
                      (overlay (text (ending-text ss) TEXT-SIZE-BIG TEXT-COLOR-GOOD)
                               BACKGROUND)
                      (overlay (text (ending-text ss) TEXT-SIZE-BIG TEXT-COLOR-BAD)
                               BACKGROUND))]
    [(sel-chapter? ss)
     (overlay (above (text (sel-chapter-text ss) TEXT-SIZE-BIG TEXT-COLOR)
                     (draw-choices (sel-chapter-sl ss) (selection-index (sel-chapter-sl ss))))
              BACKGROUND)]))

; draw-choices : SelectionList Number -> Image
; Draws the list of choices
(check-expect (draw-choices SL-STORY (selection-index SL-STORY))
              (above (text "Stay in the room." TEXT-SIZE TEXT-COLOR-SELECTED)
                     (text "Open the door." TEXT-SIZE TEXT-COLOR)))
(check-expect (draw-choices SL-STORY2 (selection-index SL-STORY2))
              (above (text "Stay in the room." TEXT-SIZE TEXT-COLOR)
                     (text "Open the door." TEXT-SIZE TEXT-COLOR-SELECTED)))

(define (draw-choices sl n)
  (cond
    [(empty? (selection-choices sl)) empty-image]
    [(= n 0) (above
              (text (choice-text (first (selection-choices sl)))
                    TEXT-SIZE TEXT-COLOR-SELECTED)
              (draw-choices
               (make-selection (rest (selection-choices sl)) (sub1 (selection-index sl)))
               (sub1 n)))]
    [else (above
           (text (choice-text (first (selection-choices sl)))
                 TEXT-SIZE TEXT-COLOR)
           (draw-choices
            (make-selection (rest (selection-choices sl)) (sub1 (selection-index sl)))
            (sub1 n)))]))

; A SelectedSection is one of
; - Ending
; - SelectedChapter

; sel-section-temp : SelectedSection -> ???
(define (sel-section-temp ss)
  (cond
    [(ending? ss) (... (ending-text ss) ... (ending-good? ss) ...)]
    [(sel-chapter? ss) (... (sel-chapter-text ss) ... (selection-temp (sel-chapter-sl ss)) ...)]))

; section->ss : Section -> SelectedSection
; Converts a Section to a SelectedSection
(check-expect (section->ss MY-STORY) (chapter->sc MY-STORY))
(check-expect (section->ss
               (make-ending "You accept the simple beauty of the void. You achieve nirvana." #t))
              (make-ending "You accept the simple beauty of the void. You achieve nirvana." #t))

(define (section->ss s)
  (cond
    [(ending? s) s]
    [(chapter? s) (chapter->sc s)]))

; story-ended? : SelectedSection -> Boolean
; Checks whether story has ended
(check-expect (story-ended? (chapter->sc MY-STORY)) #false)
(check-expect (story-ended?
               (make-ending "You accept the simple beauty of the void. You achieve nirvana." #t))
              #true)

(define (story-ended? ss)
  (cond
    [(ending? ss) #true]
    [(sel-chapter? ss) #false]))

; main/story : Section -> Section
(define (main/story s)
  (big-bang (section->ss s)
    [to-draw draw-state]
    [on-key key-handler]
    [stop-when story-ended? draw-state]))

; A PlayerSimulation is a [List-of KO]
; and represents the key events a player makes when playing a choose your own adventure game
 
; A KO (Key Option) is one of:
; - "up"
; - "down"
; - "enter"
; and represents either pressing the up arrow, down arrow, or enter/return key
 
; A Result is one of:
; - "happy"
; - "sad"
; - "incomplete"
; and represents whether a story ended happily, sadly, or did not yet end 

; ss->result : SelectedSection -> Result
; Determines the Result from a given SelectedSection
(check-expect (ss->result
               (make-ending "You accept the simple beauty of the void. You achieve nirvana." #t))
              "happy")
(check-expect
 (ss->result
  (make-ending "You scream into the void. There is no response. There never will be." #f)) "sad")
(check-expect (ss->result (chapter->sc MY-STORY)) "incomplete")

(define (ss->result ss)
  (cond
    [(ending? ss) (if (ending-good? ss)
                      "happy"
                      "sad")]
    [(sel-chapter? ss) "incomplete"]))

; run-sim : SelectedSection PlayerSimulation -> SelectedSection
; Finds the resulting SelectedSection after running PlayerSimulation on a Section
(check-expect (run-sim (chapter->sc MY-STORY) (list "enter"))
              (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f))
(check-expect (run-sim (chapter->sc MY-STORY) (list "down" "enter" "down" "enter"))
              (make-ending "You accept the simple beauty of the void. You achieve nirvana." #t))

(define (run-sim ss ps)
  (cond
    [(empty? ps) ss]
    [else (run-sim (key-handler ss (first ps)) (rest ps))]))

; simulation-result : Section PlayerSimulation -> Result
; Gets the Result of running a PlayerSimulation on a Section
(check-expect (simulation-result MY-STORY (list "enter")) "sad")
(check-expect (simulation-result MY-STORY (list "down" "enter" "down")) "incomplete")
(check-expect (simulation-result MY-STORY (list "down" "enter" "down" "enter")) "happy")
 
(define (simulation-result s ps)
  (ss->result (run-sim (section->ss s) ps)))