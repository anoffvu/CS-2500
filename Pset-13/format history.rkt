;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |format history|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct request-needed [feedback history])
(define-struct request-made [feedback history])
(define-struct request-received [song metadata feedback history])

(define TEXT-SIZE 24)
(define TEXT-COLOR "red")

(define FEEDBACKSTRING-EMPTY "")
(define FEEDBACKSTRING-DISLIKE "dislike")
(define FEEDBACKSTRING-LIKE "like")
(define FEEDBACKSTRING-NONE "none")



(define-struct songhistory [id title timesplayed])
; A SongHistory is a (make-songhistory String Nat)
; where id is the id of a song given from the server
; and title is the title of a song given from the server
; and timesplayed is the number of times the song has been played
; Interpretation : the id, title and number of times played of a given song
; Examples:
(define SONGHISTORY-1 (make-songhistory 4391 "iSpy" 1))
(define SONGHISTORY-2 (make-songhistory 9192 "Bouncin" 2))
(define SONGHISTORY-3 (make-songhistory 5555 "8teen" 3))
(define SONGHISTORY-4 (make-songhistory 1927 "Bank Account" 2))

; Template:
#;
(define (songhistory-temp sh)
  (... (songhistory-id sh) ... (songhistory-title sh) ... (songhistory-timesplayed sh) ...))

; A History is a [List-of SongHistory]
; Interpretation : a history of all the songs and the number of times they were played
; Examples:
(define HISTORY-0 (list ))
(define HISTORY-1 (list SONGHISTORY-1))
(define HISTORY-2 (list SONGHISTORY-1 SONGHISTORY-2))
(define HISTORY-3 (list SONGHISTORY-1 SONGHISTORY-2 SONGHISTORY-3))
(define HISTORY-4 (list SONGHISTORY-1 SONGHISTORY-2 SONGHISTORY-3 SONGHISTORY-4))
; Template:
#;
(define (history-temp h)
  (cond
    [(empty? h) ... h ...]
    [(cons? h) ... (songhistory-temp (first h)) ... (history-temp (rest h)) ...]))

(define MP-INIT (make-request-needed FEEDBACKSTRING-EMPTY HISTORY-0))
(define MP-0 (make-request-needed FEEDBACKSTRING-LIKE HISTORY-1))
(define MP-1 (make-request-made FEEDBACKSTRING-EMPTY HISTORY-1))
(define MP-2 (make-request-made FEEDBACKSTRING-LIKE HISTORY-2))
(define MP-3 (make-request-received "hi" "yuh" FEEDBACKSTRING-EMPTY HISTORY-3))
(define MP-4 (make-request-received "hi" "yuh" FEEDBACKSTRING-LIKE HISTORY-4))

; format-history : MusicPlayer -> Image
; stacks all the song histories on top of each other
; with and (above (foldr (text ... of each song history)
(define (format-history mp)
  (local [(define HISTORY (grab-history mp))
          (define HISTORY-AS-LIST-OF-IMAGES (history->images HISTORY))]     
  (foldr above empty-image HISTORY-AS-LIST-OF-IMAGES)))

; grab-history : MusicPlayer -> History
; grabs the history of a music player

(check-expect (grab-history MP-1) HISTORY-1)
(check-expect (grab-history MP-2) HISTORY-2)
(check-expect (grab-history MP-3) HISTORY-3)

(define (grab-history mp)
  (cond
    [(request-needed? mp) (request-needed-history mp)]
    [(request-made? mp) (request-made-history mp)]
    [(request-received? mp) (request-received-history mp)]))

; creats a constant for a history as just a list of images
(define (history->images history)
  (local [(define (history->los history)
            (local [; turns a song history into a string
                    (define (textify sh)
                      (string-append (songhistory-title sh) " " (number->string
                                                                 (songhistory-timesplayed sh))))]
            (map textify history)))
          ; creates a constant that turns a history into a list of strings where eachs tring is the
          ; title and times played of a song history
          (define LIST-OF-HISTORY-AS-STRINGS (history->los history))
          ; turns the string into an image with TEXT-SIZE and TEXT-COLOR
          (define (text-function s)
            (text s TEXT-SIZE TEXT-COLOR))]
  (map text-function LIST-OF-HISTORY-AS-STRINGS)))
