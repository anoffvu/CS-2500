;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Pset 11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;ex 1
; even-squares : [List-of Number] -> [List-of Number]
; takes a list of numbers and returns the squares of all even numbers in it
(check-expect (even-squares (list 0 2 4 6 9)) (list 0 4 16 36))
(check-expect (even-squares '()) '())
(check-expect (even-squares (list 1 3 5)) '())

(define (even-squares lon)
  (local [; even-list : [List-of Numbers] -> [List-of Numbers]
          ; takes a list and returns a list of all its even numbers
          ; given (list 0 1 2 3 4); returns (list 0 2 4)
          ; given (list 1 3 5 7); returns '()
          (define (even-list lon)
            (filter even? lon))
          ; square: Number -> Number
          ; takes in a number and squares it
          ; given 2; returns 4
          ; given 0; returns 0
          (define (square num)
            (* num num))]
    (map square (even-list lon))))


;ex 2

; even-squares.2 : [List-of Number] -> [List-of Number]
; takes a list of numbers and returns the squares of all even numbers in it
(check-expect (even-squares.2 (list 0 2 4 6 9)) (list 0 4 16 36))
(check-expect (even-squares.2 '()) '())
(check-expect (even-squares.2 (list 1 3 5)) '())

(define (even-squares.2 lon)
  (local [; square-if-even : Number [List-of Number] -> [List-of Number]
          ; if a number is even, squares it and adds it to the list
          ; given 4 (list 2 4 5); returns (list 16 2 4 5)
          ; given 3 (list 2 4 5); returns (list 2 4 5)
          (define (square-if-even num base)
            (if (even? num) (cons (* num num) base) base))]
    (foldr square-if-even '() lon)))


;ex 3
;used for a check-expect
(define (square x)
  (* x x))
; map-filter : (X Y) [List-of X] [X -> Boolean] [X -> Y] -> [List-of Y]
(check-expect (map-filter (list 0 2 4 6 9) even? square) (list 0 4 16 36))
(check-expect (map-filter '() even? square) '())
(check-expect (map-filter (list 1 3 5) even? square) '())
(check-expect (map-filter (list 1 3 5) odd? add1) (list 2 4 6))
(check-expect (map-filter (list 0 2 4 6 9) even? sub1) (list -1 1 3 5))

(define (map-filter lox pred fxn)
  (map fxn (filter pred lox)))

; even-squares.3 : [List-of Number] -> [List-of Number]
; takes a list of numbers and returns the squares of all even numbers in it
(check-expect (even-squares.3 (list 0 2 4 6 9)) (list 0 4 16 36))
(check-expect (even-squares.3 '()) '())
(check-expect (even-squares.3 (list 1 3 5)) '())

(define (even-squares.3 lon)
  (map-filter lon even? square))


;ex 4

(define-struct video [title runtime])

; A Video is a (make-video String Number)
; Interpretation: represents a video with a title and runtime
; - title is the title of the video
; - runtime is the runtime of the video in seconds
(define VIDEO-1 (make-video "vid one" 30))
(define VIDEO-2 (make-video "vid two" 90))
(define VIDEO-3 (make-video "vid three" 100))
(define VIDEO-4 (make-video "vid four" 600))
(define VIDEO-5 (make-video "vid five" 601))

; video-temp : Video -> ???
#;
(define (video-temp v)
  ... (video-title v) ... (video-runtime v)...)

; A List of Videos (LoV) is one of:
; - '()
; - (cons Video LoV)
; Interpretation: represents a list of Videos
(define LOV-0 '())
(define LOV-1 (cons VIDEO-1 '()))
(define LOV-2 (cons VIDEO-2 (cons VIDEO-1 '())))
(define LOV-3 (cons VIDEO-3 (cons VIDEO-2 (cons VIDEO-1 '()))))

#;
(define (lov-temp lov)
  (cond [(empty? lov) ...]
        [(cons? lov) ... (video-temp (first lov)) ... (lov-temp (rest lov))]))

(define-struct playlist [title videos])
; A Playlist is a (make-playlist String LoV)
; - title is the title of the playlist
; - videos is the passed in List of Videos
; Interpretation: represents a playlist with a title and a list of videos
(define PLAYLIST-0 (make-playlist "zero pl" LOV-0))
(define PLAYLIST-1 (make-playlist "one pl" LOV-1))
(define PLAYLIST-2 (make-playlist "two pl" LOV-2))
(define PLAYLIST-3 (make-playlist "three pl" LOV-3))

(define PLLIST-0 (list PLAYLIST-0))
(define PLLIST-1 (list PLAYLIST-0 PLAYLIST-1))
(define PLLIST-2 (list PLAYLIST-0 PLAYLIST-1 PLAYLIST-2))
; playlist-combiner : String [List-of Playlists] -> Playlist
; combines all the videos in a [List-of Playlists] and creates a new playlist with a new title

(check-expect (playlist-combiner "hi" PLLIST-0) (make-playlist "hi" LOV-0))
(check-expect (playlist-combiner "hi" PLLIST-1) (make-playlist "hi" LOV-1))
(check-expect (playlist-combiner "hi" PLLIST-2) (make-playlist "hi" (list VIDEO-1 VIDEO-2 VIDEO-1)))

(define (playlist-combiner title lop)
  (local [; grab-videos : Playlist [List-of Videos] -> [List-of Videos]
          ; takes the videos from a playlist and adds it onto another list of videos
          ; given (make-playlist "hi" '()) and '(); returns '()
          ; given (make-playlist "hi" '()) and (list VIDEO-1); returns (list VIDEO-1)
          (define (grab-videos pl base)
            (append (playlist-videos pl) base))]
    ; append combines 2 lists
    (make-playlist title (foldr grab-videos '() lop))))

;ex 5
(check-expect (scalar-matrix 1 1) (list (list 1)))
(check-expect (scalar-matrix 2 1) (list (list 1 0) (list 0 1)))
(check-expect (scalar-matrix 3 1) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (scalar-matrix 3 0) (list (list 0 0 0) (list 0 0 0) (list 0 0 0)))

;build a list to n
;then iterate thru this list

(define (scalar-matrix m-size k)
  (local [; list-builder : NatNumber -> [List-of Number]
          ; builds the list with the required dimensions, k is put in its
          ; corresponding spot
          ; if m-size is 5 and k is 1
          ; given 5: returns (list 0 0 0 0 k)
          (define (list-builder num)
            (local [;creates a dummy list of size n
                    (define skeleton-string (make-list m-size 0))]
              (list-builder-helper skeleton-string (+ num 1) k)))]
    (build-list m-size list-builder)))

; list-builder-helper : [List-of Number] NatNumber Number -> [List-of Number]
; takes in a dummy list and creates a list of the same size, with 0 and k in its
; proper spot
(check-expect (list-builder-helper (list 1 2 3 4) 4 1) (list 0 0 0 1))
(check-expect (list-builder-helper (list 0 0 0 0 0) 4 1) (list 0 0 0 1 0))
(check-expect (list-builder-helper (list 1 2) 4 1) (list 0 0))

;test case: (list-builder-helper (list 1 2 3 4 5 6) 3 1)                                  
(define (list-builder-helper lon list-count k)
  (cond [(empty? lon) lon]
        [(cons? lon) (if (= (length lon) list-count)
                         (append (list-builder-helper (rest lon) list-count k) (list k))
                         (append (list-builder-helper (rest lon) list-count k) (list 0)))]))


;ex 6
; into : [List-of [List-of NatNumbers]] NatNumber -> NatNumber
; retrieves a value in a list at the index then uses that value
; as the index in the next list, over and over until you reach the last

(check-expect (into (list (list 3 5) (list 100 101 102 103 104 2) (list 1 1 0) (list 50 60)) 1) 50)
(check-expect (into (list (list 3 5) (list 100 101 102 1 104 2) (list 1 1 0) (list 50 60)) 0) 60)
(check-error (into (list (list 3 5) (list 100 101 102 1 104 2) (list 1 1 0) (list 50 60)) 4))

(define (into lolon n)
  (foldl list-ref n lolon))