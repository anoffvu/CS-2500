;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pset 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;ex 1
(define-struct minute [hour minute])

; A Minute is a (make-struct PositiveInteger PositiveInteger)
; a unit of time in the day
; - hour is an integer between (0, 23) representing the hour in a day
; - minute is an integer between (0, 59) representing the minute in an hour
(define NOON (make-minute 12 0))
(define THREE-PM (make-minute 15 0))
(define TWO-AM (make-minute 2 0))

#;
(define (minute-temp t)
  ... (minute-hour t) ...
  ... (minute-minute t) ...)

;ex 2
; next-minute : Minute -> Minute
; returns the next minute in the day
(check-expect (next-minute NOON)
              (make-minute 12 1))
(check-expect (next-minute THREE-PM)
              (make-minute 15 1))
(check-expect (next-minute (make-minute 23 59))
              (make-minute 0 0))
(check-expect (next-minute (make-minute 22 59))
              (make-minute 23 0))
(check-expect (next-minute (make-minute 22 58))
              (make-minute 22 59))

(define (next-minute min)
  (cond
    [(and
      (= (minute-hour min) 23)
      (= (minute-minute min) 59))
     (make-minute 0 0)]
    [(= (minute-minute min) 59)
     (make-minute (add1 (minute-hour min)) 0)]
    [(< (minute-minute min) 59)
     (make-minute (minute-hour min) (add1 (minute-minute min)))]))



;ex 3
; constants
(define BACKGROUND (empty-scene 200 200 "maroon"))
(define TEXT-SIZE 30)
(define TEXT-COLOR "white")

; draw-minute : Minute -> Image
; Displays the time on a digital clock
(check-expect (draw-minute NOON) (overlay (text "12:00pm" TEXT-SIZE TEXT-COLOR) BACKGROUND))
(check-expect (draw-minute THREE-PM) (overlay (text "3:00pm" TEXT-SIZE TEXT-COLOR) BACKGROUND))
(check-expect (draw-minute TWO-AM) (overlay (text "2:00am" TEXT-SIZE TEXT-COLOR) BACKGROUND))
(check-expect (draw-minute (make-minute 0 0)) (overlay
                                               (text "12:00am" TEXT-SIZE TEXT-COLOR)BACKGROUND))
(check-expect (draw-minute (make-minute 2 23)) (overlay
                                                (text "2:23am" TEXT-SIZE TEXT-COLOR)BACKGROUND))

(define (draw-minute min)
  (overlay (text (minute-string min) TEXT-SIZE TEXT-COLOR) BACKGROUND))

; minute-string : Minute -> String
; main handler, converts the minute to string data
(check-expect (minute-string NOON) "12:00pm")
(check-expect (minute-string THREE-PM) "3:00pm")
(check-expect (minute-string TWO-AM) "2:00am")
(check-expect (minute-string (make-minute 13 45)) "1:45pm")

(define (minute-string min)
  (cond
    [(= (minute-hour min) 0) (string-append
                              (number->string (+ (minute-hour min) 12))
                              ":" (minute-handler min) "am")]
    [(= (minute-hour min) 12) (string-append
                               (number->string (minute-hour min))
                               ":" (minute-handler min) "pm")]
    [(> (minute-hour min) 12) (string-append
                               (number->string (- (minute-hour min) 12))
                               ":" (minute-handler min) "pm")]
    [else (string-append
           (number->string (minute-hour min))
           ":" (minute-handler min) "am")]))
    
; minute-handler : Minute -> String
; formats the minutes to read like a 12 hour clock
(check-expect (minute-handler NOON) "00")
(check-expect (minute-handler THREE-PM) "00")
(check-expect (minute-handler (make-minute 1 30)) "30")
(check-expect (minute-handler (make-minute 1 3)) "03")
(check-expect (minute-handler (make-minute 1 59)) "59")

(define (minute-handler min)
  (cond
    [(< (minute-minute min) 10) (string-append "0" (number->string (minute-minute min)))]
    [else (number->string (minute-minute min))]))

;ex 4
; main/clock : Minute -> Number
; draws a Minute and cycles to the next minute every tick
(define (main/clock initialworld)
  (minutes-elapsed 
   (big-bang initialworld
     [to-draw draw-minute]
     [on-tick next-minute])))

; minutes-elapsed : Minute -> Number
; calculates the minutes that have elapsed since the last 12:00 AM
(check-expect (minutes-elapsed (make-minute 0 0)) 0)
(check-expect (minutes-elapsed TWO-AM) 120)
(check-expect (minutes-elapsed NOON) 720)
(check-expect (minutes-elapsed THREE-PM) 900)
(check-expect (minutes-elapsed (make-minute 12 12)) 732)

(define (minutes-elapsed min)
  (+ (* (minute-hour min) 60) (minute-minute min)))
