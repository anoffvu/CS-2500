;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pset 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require neu-fall18)

(define THUG-LIFE (file-as-bytes "/Users/An/Desktop/ThugLife.mp3"))
(define TONYA (file-as-bytes "/Users/An/Desktop/Tonya.mp3"))

; A FeedbackString is one of:
; - ""
; - "dislike"
; - "like"
; - "none"
; Interpretation: The feedback that the user gave to the last song played.  The string
; "none" represents that the user gave no feedback, and the string "" represents
; that no feedback has been received yet (i.e., we are playing the first song).
(define FEEDBACKSTRING-EMPTY "")
(define FEEDBACKSTRING-DISLIKE "dislike")
(define FEEDBACKSTRING-LIKE "like")
(define FEEDBACKSTRING-NONE "none")

#;
(define (feedbackstring-temp fs)
  (cond
    [(string=? fs FEEDBACKSTRING-EMPTY) ...]
    [(string=? fs FEEDBACKSTRING-DISLIKE) ...]
    [(string=? fs FEEDBACKSTRING-LIKE) ...]
    [(string=? fs FEEDBACKSTRING-NONE) ...]))
 
(define-struct player [song1 song2 feedback])

; A MusicPlayer is a (make-player String String FeedbackString)
; Interpretation: The state of the music player
; - song1 is the bytes of the current song being played
; - song2 is the bytes of the next song to played
; - feedback is the feedback received from the user for the last song played
(define MUSICPLAYER-0 (make-player "" "" FEEDBACKSTRING-EMPTY))
(define MUSICPLAYER-1 (make-player "fakesong1" "fakesong2" FEEDBACKSTRING-LIKE))
(define MUSICPLAYER-2 (make-player THUG-LIFE TONYA FEEDBACKSTRING-LIKE))
(define MUSICPLAYER-DEFAULT (make-player THUG-LIFE TONYA FEEDBACKSTRING-EMPTY))

#;
(define (musicplayer-temp mp)
  ... (player-song1 mp) ... (player-song2 mp) ... (player-feedback mp) ...)


; main/player : World -> World
; makes a simple music player
(define (main/player initialworld)
  (big-bang initialworld
    [to-draw display-player]
    [on-key key-interact]))



; diplay-player : MusicPlayer -> Image
; displays the last feedback received

(define TEXT-SIZE 14)
(define TEXT-COLOR "red")
(define WIDTH 400)
(define HEIGHT 200)
(define BACKGROUND (overlay/offset (text "Press space to play a song" TEXT-SIZE TEXT-COLOR)
                                   0 50
                                   (empty-scene WIDTH HEIGHT)))

(check-expect (display-player MUSICPLAYER-1) (overlay
                                              (text FEEDBACKSTRING-LIKE TEXT-SIZE TEXT-COLOR)
                                              BACKGROUND))
(check-expect (display-player MUSICPLAYER-0) (overlay
                                              (text FEEDBACKSTRING-EMPTY TEXT-SIZE TEXT-COLOR)
                                              BACKGROUND))
(check-expect (display-player MUSICPLAYER-DEFAULT) (overlay
                                                    (text FEEDBACKSTRING-EMPTY TEXT-SIZE TEXT-COLOR)
                                                    BACKGROUND))

(define (display-player mp)
  (overlay (text (player-feedback mp) TEXT-SIZE TEXT-COLOR) BACKGROUND))

; key-interact : World KeyEvent -> World
; plays the next song

#| cant check-expect key-interact as it incorporates a big bang world event
(check-expect (key-interact MUSICPLAYER-1 " ")
              (make-player "fakesong2" "fakesong1" "like"))
(check-expect (key-interact MUSICPLAYER-2 " ")
              (make-player TONYA THUG-LIFE FEEDBACKSTRING-LIKE))
(check-expect (key-interact MUSICPLAYER-2 "a")
              MUSICPLAYER-2)
|#

(define (key-interact mp ke)
  (cond
    [(key=? ke " ")
     (make-player (player-song2 mp) (player-song1 mp) (play-sound (player-song1 mp)))]
    [else mp]))


