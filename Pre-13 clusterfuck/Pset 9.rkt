;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Pset 9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
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


; A ClientMsg is "next"
; and represents a request for the next song

(define CM-0 "next")
(define CM-1 "next")

#;
(define (clientmsg-temp cm)
  ...)


; A Metadata is a (list String String Number String)
; - where the first String is the song's title
; - the second String is the song's artist
; - the Number is the length of the song (in seconds)
; - and the third String is the song's album

(define MD-1 (list "THUG LIFE" "BROCKHAMPTON" 123 "Iridescence"))
(define MD-2 (list "TONYA" "BROCKHAMPTON" 283 "Iridescence"))


; A SongMsg is a (list "SONG" Metadata String)
; - where the metadata is information about the given song
; - and the second the String is the actual byte-string of music

(define SONGMSG-1 (list "SONG" MD-1 THUG-LIFE))
(define SONGMSG-2 (list "SONG" MD-2 TONYA))

#;
(define (songmsg-temp s)
  ... (first s) ... (metadata-temp (second s)) ... (third s) ...)


; An ErrorMsg is a (list "ERROR" String)
; where the second string is the message from the server about what went wrong

(define ERRORMSG-0 (list "ERROR" "error 0"))
(define ERRORMSG-1 (list "ERROR" "error 1"))
(define ERRORMSG-2 (list "ERROR" "error 2"))

#;
(define (errormsg-temp e)
  ... (first e) ... (second e) ...)


; A ServerMsg is one of:
; - ErrorMsg
; - SongMsg

(define SERVERMSGERROR-1 (list "ERROR" "error 1"))
(define SERVERMSGERROR-2 (list "ERROR" "error 2"))
(define SERVERMSGSONG-1 (list "SONG" MD-1 THUG-LIFE))
(define SERVERMSGSONG-2 (list "SONG" MD-2 TONYA))

#;
(define (servermsg-temp s)
  (cond
    [(string=? (first s) "ERROR") ... (errormsg-temp s)...]
    [(string=? (first s) "SONG") ... (songmsg-temp s) ...]))


(define-struct request-needed [feedback])
(define-struct request-made [feedback])
(define-struct request-received [song metadata feedback])

; A MusicPlayer is one of:
; - (make-request-needed FeedbackString)
; - (make-request-made FeedbackString)
;   - a feedback is the feedback of the previous song
; - (make-request-received String Metadata FeedbackString)
;   - a song is the song data as bytes
;   - the metadata is the Metadata of a song
;   - a feedback is the feedback of the previous song
; MusicPlayer is a (make-request-needed ...) when a request has not been made and a song has not been
; received
; MusicPlayer is a (make-request-made ...) when a request has been made but no song has been received
; MusicPlayer is a (make-request-received ...) when a request has been made
; and the song has been received

(define MP-INIT (make-request-needed FEEDBACKSTRING-EMPTY))
(define MP-0 (make-request-needed FEEDBACKSTRING-LIKE))
(define MP-1 (make-request-made FEEDBACKSTRING-EMPTY))
(define MP-2 (make-request-made FEEDBACKSTRING-LIKE))
(define MP-3 (make-request-received THUG-LIFE MD-1 FEEDBACKSTRING-EMPTY))
(define MP-4 (make-request-received TONYA MD-2 FEEDBACKSTRING-LIKE))

#;
(define (musicplayer-temp mp)
  (cond
    [(request-needed? mp) ... (feedbackstring-temp (request-needed-feedback mp)) ...]
    [(request-made? mp) ... (feedbackstring-temp (request-made-feedback mp)) ...]
    [(request-received? mp) ... (request-received-song mp) ...
                            ... (metadata-temp (request-received-metadata mp)) ...
                            ... (feedbackstring-temp (request-received-feedback mp)) ...]))


; A Package is a (make-package MusicPlayer ClientMsg)
; - and dictacts the next state of the world as well as
; - the message sent from the client to the server

(define PACKAGE-0 (make-package MP-INIT "next"))
(define PACKAGE-1 (make-package MP-1 "next"))
(define PACKAGE-2 (make-package MP-2 "next"))
(define PACKAGE-3 (make-package MP-3 "next"))


; A PlayerResult is one of:
; - MusicPlayer
; - Package

(define PLAYERRESULT-0 MP-INIT)
(define PLAYERRESULT-1 MP-1)
(define PLAYERRESULT-2 MP-2)
(define PLAYERRESULT-3 PACKAGE-0)
(define PLAYERRESULT-4 PACKAGE-1)
(define PLAYERRESULT-5 PACKAGE-2)

#;
(define (playerresult-temp pr)
  (cond
    [(or request-needed? request-made? request-received?) ... (musicplayer-temp pr) ... ]
    [(package? pr) ... ]))


; main/player : MusicPlayer -> MusicPlayer
; makes a simple music player
(define (main/player initialplayer)
  (big-bang initialplayer
    [on-tick check-state]
    [to-draw display-player]
    [on-key key-interact]
    [on-receive receive-interact]
    [register "dictionary.ccs.neu.edu"]
    [port 10001]))


; diplay-player : MusicPlayer -> Image
; displays the last feedback received

(define TEXT-SIZE 24)
(define TEXT-COLOR "red")
(define WIDTH 400)
(define HEIGHT 200)
(define BACKGROUND (overlay/offset (text "Press space to play this song" TEXT-SIZE TEXT-COLOR)
                                   0 50
                                   (empty-scene WIDTH HEIGHT)))

(check-expect (display-player MP-INIT)
              (overlay (above
                        (text "fetching next song..." TEXT-SIZE TEXT-COLOR)
                        (text (string-append "Feedback of the last song: "
                                             (request-needed-feedback MP-INIT)) TEXT-SIZE TEXT-COLOR))
                       BACKGROUND))
(check-expect (display-player MP-1)
              (overlay (above
                        (text "fetching next song..." TEXT-SIZE TEXT-COLOR)
                        (text (string-append "Feedback of the last song: "
                                             (request-made-feedback MP-1)) TEXT-SIZE TEXT-COLOR))
                       BACKGROUND))
(check-expect (display-player MP-3)
              (overlay (above
                        (text (first (request-received-metadata MP-3)) TEXT-SIZE TEXT-COLOR)
                        (text (second (request-received-metadata MP-3)) TEXT-SIZE TEXT-COLOR)
                        (text (string-append "Feedback of the last song: "
                                             (request-received-feedback MP-3)) TEXT-SIZE TEXT-COLOR))
                       BACKGROUND))

(define (display-player mp)
  (cond
    [(request-needed? mp)
     (overlay (above
               (text "fetching next song..." TEXT-SIZE TEXT-COLOR)
               (text (string-append "Feedback of the last song: "
                                    (request-needed-feedback mp)) TEXT-SIZE TEXT-COLOR))
              BACKGROUND)]
    [(request-made? mp)
     (overlay (above
               (text "fetching next song..." TEXT-SIZE TEXT-COLOR)
               (text (string-append "Feedback of the last song: "
                                    (request-made-feedback mp)) TEXT-SIZE TEXT-COLOR))
              BACKGROUND)]
    [(request-received? mp)
     (overlay (text-formatter mp)
              BACKGROUND)]))

; text-formatter : MusicPlayer -> Image
; displays the relevant information of a MusicPlayer as an image
(check-expect (text-formatter MP-INIT) (text (request-needed-feedback MP-INIT) TEXT-SIZE TEXT-COLOR))
(check-expect (text-formatter MP-1) (text (request-made-feedback MP-1) TEXT-SIZE TEXT-COLOR))
(check-expect (text-formatter MP-3) (above
                                     (text (first (request-received-metadata MP-3))
                                           TEXT-SIZE TEXT-COLOR)
                                     (text (second (request-received-metadata MP-3))
                                           TEXT-SIZE TEXT-COLOR)
                                     (text (string-append "Feedback of the last song: "
                                                          (request-received-feedback MP-3))
                                           TEXT-SIZE TEXT-COLOR)))

(define (text-formatter mp)
  (cond
    [(request-needed? mp) (text (request-needed-feedback mp) TEXT-SIZE TEXT-COLOR)]
    [(request-made? mp) (text (request-made-feedback mp) TEXT-SIZE TEXT-COLOR)]
    [(request-received? mp) (above
                             (text (first (request-received-metadata mp))
                                   TEXT-SIZE TEXT-COLOR)
                             (text (second (request-received-metadata mp))
                                   TEXT-SIZE TEXT-COLOR)
                             (text (string-append "Feedback of the last song: "
                                                  (request-received-feedback mp))
                                   TEXT-SIZE TEXT-COLOR))]))


; check-state : MusicPlayer -> PlayerResult
; checks if the program needs to send a request to a server
(check-expect (check-state MP-INIT) (make-package
                                     (make-request-made (request-needed-feedback MP-INIT)) "next"))
(check-expect (check-state MP-1) MP-1)
(check-expect (check-state MP-3) MP-3)

(define (check-state mp)
  (cond
    [(request-needed? mp) (make-package (make-request-made (request-needed-feedback mp)) "next")]
    [(request-made? mp) mp ]
    [(request-received? mp) mp ]))


; receive-interact : MusicPlayer ServerMsg -> PlayerResult
; sends the server information to your player
(check-expect (receive-interact MP-1 SERVERMSGERROR-1) (make-package MP-1 "next"))
(check-expect (receive-interact MP-3 SERVERMSGSONG-1) (make-request-received 
                                                       (third SERVERMSGSONG-1)
                                                       (second SERVERMSGSONG-1)
                                                       (request-received-feedback MP-3)))

(define (receive-interact mp sm)
  (cond
    [(string=? (first sm) "ERROR") (make-package mp "next")]
    [(string=? (first sm) "SONG") (song-interact mp sm)]))


; song-interact : MusicPlayer ServerMsg -> MusicPlayer
; takes in the initial world and creates the resultant world given by ServerMsg
(check-expect (song-interact MP-INIT SERVERMSGSONG-1) (make-request-received 
                                                       (third SERVERMSGSONG-1)
                                                       (second SERVERMSGSONG-1)
                                                       (request-needed-feedback MP-INIT)))
(check-expect (song-interact MP-1 SERVERMSGSONG-1) (make-request-received 
                                                    (third SERVERMSGSONG-1)
                                                    (second SERVERMSGSONG-1)
                                                    (request-made-feedback MP-1)))
(check-expect (song-interact MP-3 SERVERMSGSONG-1) (make-request-received 
                                                    (third SERVERMSGSONG-1)
                                                    (second SERVERMSGSONG-1)
                                                    (request-received-feedback MP-3)))
(check-expect (song-interact MP-3 SERVERMSGSONG-2) (make-request-received 
                                                    (third SERVERMSGSONG-2)
                                                    (second SERVERMSGSONG-2)
                                                    (request-received-feedback MP-3)))

(define (song-interact mp sm)
  (cond
    [(request-needed? mp) (make-request-received 
                           (third sm)
                           (second sm)
                           (request-needed-feedback mp))]
    [(request-made? mp) (make-request-received 
                         (third sm)
                         (second sm)
                         (request-made-feedback mp))]
    [(request-received? mp) (make-request-received 
                             (third sm)
                             (second sm)
                             (request-received-feedback mp))]))

  

; key-interact : World KeyEvent -> World
; plays the next song
;cant check-expect key-interact as it incorporates a big bang world event

(define (key-interact mp ke)
  (if (key=? ke " ")
      (key-helper mp)
      mp))


; key-helper : MusicPlayer -> Package
; turns a MusicPlayer into a Package
(check-expect (key-helper MP-0) MP-0)
(check-expect (key-helper MP-1) MP-1)
;(check-expect (key-helper MP-3) ...
; cant check-expect a play-sound

(define (key-helper mp)
  (cond
    [(request-needed? mp) mp]
    [(request-made? mp) mp]
    [(request-received? mp) (make-request-needed
                             (play-sound (request-received-song mp)))]))
