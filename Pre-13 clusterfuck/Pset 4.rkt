;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pset 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 1

(define-struct songdata [currentsong nextsong])

; A SongData is a (make-songdata String String)
; Interpretation:
; - The first field is the current song
; - The second field is the next song
; Examples:
(define SONG-1 (make-songdata "BLEACH" "Ocean Eyes"))
(define SONG-2 (make-songdata "Sunflower Seeds" "Reborn"))
(define SONG-3 (make-songdata "" ""))

#;
(define (songdata-temp sd)
  ... (songdata-currentsong sd) ...
  ... (songdata-nextsong sd) ...)

; A Feedback is one of:
; - "like"
; - "dislike"
; - "none"
; Interpretation: the rating of each song by the user
; Examples:
(define FEEDBACK-LIKE "like")
(define FEEDBACK-DISLIKE "dislike")
(define FEEDBACK-NONE "none")

#;
(define (feedback-temp fb)
  (cond
    [(string=? FEEDBACK-LIKE fb) ...]
    [(string=? FEEDBACK-DISLIKE fb) ...]
    [(string=? FEEDBACK-NONE fb) ...]))


; Exercise 2

; A Position is a (make-posn Int Int)
; Interpretation: position of a point on a coordinate plane
; Exaples:
(define POSITION-1 (make-posn 0 0))
(define POSITION-2 (make-posn 3 -3))

#;
(define (position-temp p)
  ... (posn-x p) ...
  ... (posn-y p) ...)

; manhattan-distance : Position Position -> Number
; takes two Positions and returns the distance between them

(check-expect (manhattan-distance (make-posn 0 0) (make-posn 1 1)) 2)
(check-expect (manhattan-distance (make-posn 0 0) (make-posn 30 0)) 30)
(check-expect (manhattan-distance (make-posn 30 30) (make-posn 50 40)) 30)
(check-expect (manhattan-distance (make-posn -20 0) (make-posn 20 0)) 40)

(define (manhattan-distance pos1 pos2) 
  (+ (abs (- (posn-x pos1) (posn-x pos2)))
     (abs (- (posn-y pos1) (posn-y pos2)))))


; Exercise 3

(define-struct planet [name radius rings])
; A Planet is a (make-planet String Number Int)
; and represents a planet's name, radius (in km), and # of rings
; Examples:

(define EARTH (make-planet "Earth" 6371 0))
(define SATURN (make-planet "Saturn" 58232 7))
(define BIGBOY (make-planet "Big Boy" 99999 9))

; make-planet : String Number Int -> Planet

; planet-name : Planet -> String

; planet-radius : Planet -> Number

; planet-rings : Planet -> Int

; planet? : ANY -> Boolean


; Exercise 4

#;
(define (planet-temp pl)
  ... (planet-name pl) ...
  ... (planet-radius pl) ...
  ... (planet-rings pl) ...)


; Exercise 5
; ring-discovered : Planet -> Planet
; adds one ring to a given planet

(check-expect (ring-discovered EARTH)
              (make-planet "Earth" 6371 1))
(check-expect (ring-discovered SATURN)
              (make-planet "Saturn" 58232 8))
(check-expect (ring-discovered BIGBOY)
              (make-planet "Big Boy" 99999 10))

(define (ring-discovered pl)
  (make-planet (planet-name pl) (planet-radius pl) (+ 1 (planet-rings pl))))


; Exercise 6
; bigger-planet : Planet Planet -> String
; returns the Planet name with the bigger radius

(check-expect (bigger-planet EARTH SATURN) "Saturn")
(check-expect (bigger-planet BIGBOY SATURN) "Big Boy")
(check-expect (bigger-planet SATURN SATURN) "Tied: Saturn - Saturn")

(define (bigger-planet p1 p2)
  (cond
    [(< (planet-radius p1) (planet-radius p2)) (planet-name p2)]
    [(> (planet-radius p1) (planet-radius p2)) (planet-name p1)]
    [else (string-append "Tied: " (planet-name p1) " - " (planet-name p2))]))

 