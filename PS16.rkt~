;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname PS16) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Problem Set 16

; NOTES:
; gets list of songs as soon as the program starts (first state, requests metadatamsg)
; when in the second state, nothing happens, waiting for song or metadata list, etc
; when you press enter, brings you into the state where you have a song ready to play (third)
; songmsg/errormsg is received from the server once a specific song is requested from the list
;         (w/ enter), but not metadatamsg 

; NEEDED:
; function that transforms IDMetaDataPair into Song, adding in bystestring from request
; in MPState data def, add another state where you have the list of available songs
; in MP data def, add the current index into the struct to work properly with modulo/choosing songs
; play screen with list of songs -> have each song name then next to it, playcount
; ADD CLAUSE IN KEY-HANDLER TO NOT RESPOND TO KEYS BEFORE USER HAS SONG OPTIONS



(require 2htdp/universe)
(require 2htdp/image)
(require neu-fall18)
(require 2htdp/batch-io)


(define-struct song [title artist length album bytestring id])
; a Song is a (make-song String String Number String String)
; Interpretation: represents the data of a song
; - title is the title of the song
; - artist is the song's artist
; - length is the song's length in seconds
; - album is the song's album
; - bytestring is the actual bytes of data of the song
; - id is the id of the song given from the server
; Examples:
(define SONG-1 (make-song "BankAccount" "Savage" 155 "issa" "xxx" 2929))
(define SONG-2 (make-song "BetterNow" "Posty" 201 "bandb" "yyy" 8573))
(define SONG-3 (make-song "8teen" "Khalid" 255 "American Teen" "zzz" 7163))
  
; Template:
; song-temp : Song -> ?
#;
(define (song-temp s)
  (... (song-title s) ...
       ... (song-artist s) ...
       ... (song-length s) ...
       ... (song-album s) ...
       ... (song-bytestring s) ...
       ... (song-id s) ...))

(define-struct songhistory [song timesplayed])
; A SongHistory is a (make-songhistory Song Number)
; Interpretation : the data of a song and the number of times it has been played
; where song is the song that was just played
; and timesplayed is the number of times the song has been played
; Examples:
(define SONGHISTORY-1 (make-songhistory SONG-1 1))
(define SONGHISTORY-2 (make-songhistory SONG-2 2))
(define SONGHISTORY-3 (make-songhistory SONG-3 3))

; Template:
#;
(define (songhistory-temp sh)
  (... (song-temp (songhistory-song sh)) ... (songhistory-timesplayed sh) ...))

; A History is a [List-of SongHistory]
; Interpretation : a history of all the songs and the number of times they were played
; Examples:
(define HISTORY-0 (list ))
(define HISTORY-1 (list SONGHISTORY-1))
(define HISTORY-2 (list SONGHISTORY-1 SONGHISTORY-2))
(define HISTORY-3 (list SONGHISTORY-1 SONGHISTORY-2 SONGHISTORY-3))
; Template:
#;
(define (history-temp h)
  (cond
    [(empty? h) ... h ...]
    [(cons? h) ... (songhistory-temp (first h)) ... (history-temp (rest h)) ...]))

         
                    
; a MusicPlayerState is one of:
; - #false
;   - represents that the program has started and a song has not been requested yet
; - "requested"
;   - when a song has been requested from the server but not yet received
; - (make-song String String Number String String) 
;   - when a requested song has been received from the server
; Interpretation: the current state of a music player
; Examples:
(define MPSTATE-1 #false)
(define MPSTATE-2 "requested")
(define MPSTATE-3 (make-song "yo" "yo" 222 "o" "oo" 12))
(define MPSTATE-4 SONG-1)
(define MPSTATE-5 SONG-2)
; Template:
; mpstate-temp: MusicPlayerState -> ?
#;
(define (mpstate-temp mps)
  (cond
    [(boolean? mps) ...]
    [(string? mps) ...]
    [(song? mps) ... (song-temp mps) ...])) 


(define-struct player [mpstate history feedback])
; A MusicPlayer is a (make-player MusicPlayerState [List-of SongHistory] FeedbackString)
; Interpretation: A music player with a current state and a feedback 
; - where mpstate is the current state of the music player
; - history is the list of song histories of the player
; - and feedback is the feedback received from the user for the last song played
; Examples:
(define MUSICPLAYER-1 (make-player MPSTATE-1 (list ) ""))
(define MUSICPLAYER-2 (make-player MPSTATE-2 (list ) ""))
(define MUSICPLAYER-3 (make-player MPSTATE-3 HISTORY-1 ""))
(define MUSICPLAYER-4 (make-player MPSTATE-4 HISTORY-1 "dislike"))
(define MUSICPLAYER-5 (make-player MPSTATE-5 HISTORY-3 "like"))
; Template:
; musicplayer-temp: MusicPlayer -> ?
#;
(define (musicplayer-temp mp)
  ... (mpstate-temp (player-mpstate mp)) ...
  ... (history-temp (player-history mp)) ...
  ... (feedback-temp (player-feedback mp) ...))


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
; Template:
; feedbackstring-temp : FeedbackString -> ?
#;
(define (feedbackstring-temp fs)
  (cond
    [(string=? fs FEEDBACKSTRING-EMPTY) ...]
    [(string=? fs FEEDBACKSTRING-DISLIKE) ...]
    [(string=? fs FEEDBACKSTRING-LIKE) ...]
    [(string=? fs FEEDBACKSTRING-NONE) ...]))

 
; A Package is a (make-package MusicPlayer ClientMsg)
; - and dictacts the next state of the world as well as
; - the message sent from the client to the server
(define PACKAGE-1 (make-package MUSICPLAYER-1 "next"))
(define PACKAGE-2 (make-package MUSICPLAYER-2 "next"))


; A ClientMsg is "next"
; and represents a request for the next song
(define CLIENTMSG "next")

; A Metadata is a (list String String Number String)
; Interpretation: the data pertaining to a song
; - where the first String is the song's title
; - the second String is the song's artist
; - the Number is the length of the song (in seconds)
; - and the third String is the song's album
; Examples
(define METADATA-1 (list "BankAccount" "Savage" 155 "issa"))
(define METADATA-2 (list "BetterNow" "Posty" 201 "bandb"))
; Template:
; metadata-temp: Metadata -> ?
#;
(define (metadata-temp md)
  [(empty? md) ...]
  [(cons? md)
   ... (first md) ... (second md) ... (third md) ... (fourth md) ...])


; A SongMsg is a (list "SONG" Nat Metadata String)
; - where the Nat is the song's unique ID#
; - the Metadata is information about the song
; - and the String is the actual byte-string of music
; Interpretation: The given song from the server, including its ID#, Metadata bytes
(define SONGMSG-1 (list "SONG" 2929 METADATA-1 "xxx"))
(define SONGMSG-2 (list "SONG" 8573 METADATA-2 "yyy"))


; An ErrorMsg is a (list "ERROR" String)
; where the second string is the message from the server about what went wrong
; Examples:
(define ERRORMSG-1 (list "ERROR" "server too busy"))
(define ERRORMSG-2 (list "ERROR" "a song cannot be found"))


; A ServerMsg is one of:
; - ErrorMsg
; - SongMsg
; Interpretation: the response received from the server
; Examples:
(define SERVERMSG-1 ERRORMSG-1)
(define SERVERMSG-2 ERRORMSG-2)
(define SERVERMSG-3 SONGMSG-1)
(define SERVERMSG-4 SONGMSG-2)
; Template
; servermsg-temp: ServerMsg -> ?
#;
(define (servermsg-temp sm)
  (cond
    [(string=? (first sm) "ERROR") ...]
    [(string=? (first sm) "SONG") ...]))


; A PlayerResult is one of:
; - MusicPlayer
; - Package
                    
; Exercise 3

(define FILEPATH "history.csv")
                          
; main/player : Any -> String
; world program; world is a Musicplayer
; runs a music player that plays songs from the server and displays the user
; feedback from the last song played, the song's title, artist, and album, and a list of
; songs played with the # of times they have been played
(define (main/player _)
  (save-history FILEPATH
                (player-history 
                 (big-bang INITIAL-PLAYER
                   [register "dictionary.ccs.neu.edu"]
                   [port 10001]
                   [to-draw draw-player]
                   [on-tick request-song]
                   [on-key handle-space]
                   [on-receive receive-msg]))))

; format-csv-data : [List-of String] -> SongHistory
; transforms the CSV list of strings into a SongHistory
(check-expect (format-csv-data (list "10" "title1" "300"))
              (make-songhistory (make-song "title1" "" 0 "" "" 10) 300))
(check-expect (format-csv-data (list "1000" "hi" "0000"))
              (make-songhistory (make-song "hi" "" 0 "" "" 1000) 0000))
(define (format-csv-data los)
  (make-songhistory (make-song (second los) "" 0 "" "" (string->number (first los))) (string->number
                                                                                      (third los))))

; read-history : String -> History
; reads a CSV and creates a History out of it

(define (read-history fpath)
  (map format-csv-data (read-csv-file fpath)))

; read-file-if-exists : String -> History
; reads a CSV and creates a History out of it if it exists
(check-expect (read-file-if-exists "this file should not exist please") '())
(define (read-file-if-exists fpath)
  (if (file-exists? fpath)
      (read-history fpath)
      '()))

(define INITIAL-PLAYER (make-player MPSTATE-1 (read-file-if-exists FILEPATH) FEEDBACKSTRING-EMPTY))

              

; request-song : MusicPlayer -> PlayerResult 
; requests a song from the server when the player is in its initial state
(check-expect (request-song MUSICPLAYER-1) (make-package MUSICPLAYER-2 CLIENTMSG))
(check-expect (request-song MUSICPLAYER-2) MUSICPLAYER-2)
(check-expect (request-song MUSICPLAYER-3) MUSICPLAYER-3)
 
(define (request-song mp)
  (cond
    [(boolean? (player-mpstate mp))
     (make-package (make-player "requested" (player-history mp) (player-feedback mp)) CLIENTMSG)]
    [(string? (player-mpstate mp)) mp]
    [(song? (player-mpstate mp)) mp]))


; receive-msg : MusicPlayer ServerMsg -> PlayerResult
; updates the state of the player after receiving a message from the server
; if it receives an error, then it makes another request to the server

(check-expect (receive-msg MUSICPLAYER-2 SERVERMSG-1) (make-package MUSICPLAYER-2 CLIENTMSG))
(check-expect (receive-msg MUSICPLAYER-2 SERVERMSG-2) (make-package MUSICPLAYER-2 CLIENTMSG))
(check-expect (receive-msg MUSICPLAYER-2 SERVERMSG-3) (make-player SONG-1 HISTORY-0 ""))

(define (receive-msg mp sm)
  (cond
    [(string=? (first sm) "ERROR") (make-package MUSICPLAYER-2 CLIENTMSG)]
    [(string=? (first sm) "SONG") (make-player (convert-to-song sm) (player-history mp)
                                               (player-feedback mp))]))


; convert-to-song ; SongMsg -> Song
; extracts metadata and bytestring from a SongMsg and converts this data to a Song struct
(check-expect (convert-to-song SONGMSG-1) SONG-1)
(check-expect (convert-to-song SONGMSG-2) SONG-2)

(define (convert-to-song sm)
  (make-song (first (third sm))
             (second (third sm))
             (third (third sm))
             (fourth (third sm))
             (fourth sm)
             (second sm)))

; handle-space : MusicPlayer KeyEvent -> MusicPlayer
; plays the received song and awaits the feedback when the user presses the space bar
(check-expect (handle-space MUSICPLAYER-3 "k") MUSICPLAYER-3)
(check-expect (handle-space MUSICPLAYER-4 "p") MUSICPLAYER-4)
(check-expect (handle-space MUSICPLAYER-2 "a") MUSICPLAYER-2)

; no further check-expects are possible because play-sound calls big-bang 

(define (handle-space mp ke)
  (cond
    [(and (key=? ke " ") (song? (player-mpstate mp)))
     (handle-feedback mp)]
    [else mp]))

; handle-feedback-and-history : MusicPlayer -> MusicPlayer
; replaces the prior feedback in the MusicPlayer with the new feedback, 
; updates the History, and sets the MusicPlayer back to its inital state

; no check expects possible because play-sound calls big-bang

(define (handle-feedback mp)
  (make-player #false (history-updater (player-mpstate mp) (player-history mp))
               (play-sound
                (song-bytestring (player-mpstate mp)))))


; history-updater : Song History -> History
; updates the history after a song has been played
(check-expect (history-updater SONG-1 HISTORY-0) HISTORY-1)
(check-expect (history-updater SONG-1 HISTORY-1) (list (make-songhistory SONG-1 2)))
(check-expect (history-updater SONG-2 HISTORY-2) (list SONGHISTORY-1 (make-songhistory SONG-2 3)))
(define (history-updater song history)
  (local [; history-contains? : Song History -> Boolean
          ; determines if the given song appears in any song history in a History
          ; given SONG-1 and HISTORY-0, return #false
          ; given SONG-1 and HISTORY-1, return #true
          ; given SONG-2 and HISTORY-2, return #true
          ; given SONG-1 and HISTORY-1, return #false
          (define (history-contains? song history-other)
            (local [;song-in-history? : SongHistory -> Boolean
                    ; is the given song in a certain SongHistory? 
                    ; if SONG-1, and given SONGHISTORY-1, return #true
                    ; if SONG-3, and given SONGHISTORY-3, return #false
                    (define (song-in-history? sh)
                      (= (song-id song) (song-id (songhistory-song sh))))]
              (ormap song-in-history? history-other)))]
    (if (history-contains? song history)
        (increment-songhistory song history)
        (cons (make-songhistory song 1) history))))


; increment-songhistory : Song History -> History 
; Increments the count of the timesplayed in a SongHistory by 1 after a certain song has been played
(check-expect (increment-songhistory SONG-1 HISTORY-1) (list (make-songhistory SONG-1 2)))
(check-expect (increment-songhistory SONG-2 HISTORY-2) (list SONGHISTORY-1
                                                             (make-songhistory SONG-2 3)))
(define (increment-songhistory song history)
  (local [; increment-songhist? : SongHistory -> Boolean
          ; should timesplayed in a given SongHistory be incremented by 1?
          ; if SONG-1 is used, and SONGHISTORY-1 is given, return #true
          ; if SONG-3 is used, and SONGHISTORY-2 is given, return #false
          (define (increment-songhist? sh)
            (= (song-id song) (song-id (songhistory-song sh))))
    
          ; increment-timesplayed : SongHistory -> SongHistory
          ; increments the timesplayed of a given SongHistory by 1 if increment-songhist? returns #t
          ; SONGHISTORY-1 is given, return (make-songhistory SONG-1 2)
          ; SONGHISTORY-2 is given, return (make-songhistory SONG-2 3)
          ; SONGHISTORY-3 is given, return (make-songhistory SONG-3 4)
          (define (increment-timesplayed sh)
            (if (increment-songhist? sh)
                (make-songhistory song (add1 (songhistory-timesplayed sh)))
                sh))]
    (map increment-timesplayed history)))
    
                                       
; Image Constants
(define BACKGROUND (empty-scene 400 300 "Black"))
(define TEXT-SIZE 20)
(define TEXT-COLOR "green")
(define INSTRUCTIONS (place-image
                      (text "Press space to play the song" TEXT-SIZE TEXT-COLOR) 200 150 BACKGROUND))

(define HISTORY-TEXT (text "History | Times Played:" TEXT-SIZE TEXT-COLOR))
; draw-player : MusicPlayer -> Image
; displays instructions on how to play a song and renders the feedback from the user,
; the upcoming song's title, artist, album, along with the history of songs played, w/ # timesplayed
(check-expect (draw-player MUSICPLAYER-1)
              (place-image (waiting-image MUSICPLAYER-1) 200 150 BACKGROUND))
(check-expect (draw-player MUSICPLAYER-2)
              (place-image (waiting-image MUSICPLAYER-2) 200 150 BACKGROUND))
(check-expect (draw-player MUSICPLAYER-3)
              (place-image (above HISTORY-TEXT (format-history MUSICPLAYER-3)) 200 50
                           (place-image (draw-mpdata MUSICPLAYER-3) 200 200 INSTRUCTIONS)))
(check-expect (draw-player MUSICPLAYER-4)
              (place-image (above HISTORY-TEXT (format-history MUSICPLAYER-4)) 200 50
                           (place-image (draw-mpdata MUSICPLAYER-4) 200 200 INSTRUCTIONS)))
(check-expect (draw-player MUSICPLAYER-5)
              (place-image (above HISTORY-TEXT (format-history MUSICPLAYER-5)) 200 50
                           (place-image (draw-mpdata MUSICPLAYER-5) 200 200 INSTRUCTIONS)))

(define (draw-player mp)
  (cond
    [(or (string? (player-mpstate mp)) (boolean? (player-mpstate mp)))
     (place-image (waiting-image mp) 200 150 BACKGROUND)]
    [(song? (player-mpstate mp))
     (place-image (above HISTORY-TEXT (format-history mp)) 200 50
                  (place-image (draw-mpdata mp) 200 200 INSTRUCTIONS))]))

; waiting-image : MusicPlayer -> Image
; creates an image that is shown when the player is waiting for
; a song to be requested or received from the server

(check-expect (waiting-image (make-player #false HISTORY-0 "dislike"))
              (above (text "Waiting..." TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Feedback: " "dislike") TEXT-SIZE TEXT-COLOR)))
(check-expect (waiting-image (make-player "requested" HISTORY-0 "none"))
              (above (text "Waiting..." TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Feedback: " "none") TEXT-SIZE TEXT-COLOR)))
(check-expect (waiting-image MUSICPLAYER-3)
              (above (text "Waiting..." TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Feedback: " "") TEXT-SIZE TEXT-COLOR)))

(define (waiting-image mp)
  (above (text "Waiting..." TEXT-SIZE TEXT-COLOR)
         (text (string-append "Feedback: " (player-feedback mp)) TEXT-SIZE TEXT-COLOR)))


; draw-mpdata: MusicPlayer -> Image
; creates an image that shows the feedback from the user, and the upcoming song's
; title, artist, album, and history of songs played with # timesplayed for each of them
(check-expect (draw-mpdata MUSICPLAYER-3)
              (above (text (string-append "Feedback: " "") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Up Next: " "yo") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Artist: " "yo") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Album: " "o") TEXT-SIZE TEXT-COLOR)))

(check-expect (draw-mpdata MUSICPLAYER-4)
              (above (text (string-append "Feedback: " "dislike") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Up Next: " "BankAccount") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Artist: " "Savage") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Album: " "issa") TEXT-SIZE TEXT-COLOR)))

(check-expect (draw-mpdata MUSICPLAYER-5)
              (above (text (string-append "Feedback: " "like") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Up Next: " "BetterNow") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Artist: " "Posty") TEXT-SIZE TEXT-COLOR)
                     (text (string-append "Album: " "bandb") TEXT-SIZE TEXT-COLOR)))

(define (draw-mpdata mp)
  (above (text (string-append "Feedback: " (player-feedback mp)) TEXT-SIZE TEXT-COLOR)
         (text (string-append "Up Next: " (song-title (player-mpstate mp))) TEXT-SIZE TEXT-COLOR)
         (text (string-append "Artist: " (song-artist (player-mpstate mp))) TEXT-SIZE TEXT-COLOR)
         (text (string-append "Album: " (song-album (player-mpstate mp))) TEXT-SIZE TEXT-COLOR)))


; format-history : MusicPlayer -> Image
; stacks all the song histories on top of each other
; with and (above (foldr (text ... of each song history)
(check-expect (format-history MUSICPLAYER-1) empty-image)
(check-expect (format-history MUSICPLAYER-2) empty-image)
(check-expect (format-history MUSICPLAYER-4) (above empty-image
                                                    (text "BankAccount 1" TEXT-SIZE TEXT-COLOR)))
(check-expect (format-history MUSICPLAYER-5) (above empty-image
                                                    (text "BankAccount 1" TEXT-SIZE TEXT-COLOR)
                                                    (text "BetterNow 2" TEXT-SIZE TEXT-COLOR)
                                                    (text "8teen 3" TEXT-SIZE TEXT-COLOR)))

(define (format-history mp)
  (local [; makes the list of images created by history->images as a constant
          (define HISTORY-AS-LIST-OF-IMAGES (history->images (player-history mp)))]    
    (foldr above empty-image HISTORY-AS-LIST-OF-IMAGES)))


; history->images : History -> [List-of Images]
; takes in a history and represents each song history as an image
(check-expect (history->images HISTORY-0) (list empty-image))
(check-expect (history->images HISTORY-1)
              (list (text "BankAccount 1" TEXT-SIZE TEXT-COLOR)))
(check-expect (history->images HISTORY-2)
              (list (text "BankAccount 1" TEXT-SIZE TEXT-COLOR)
                    (text "BetterNow 2" TEXT-SIZE TEXT-COLOR)))
(check-expect (history->images HISTORY-3)
              (list (text "BankAccount 1" TEXT-SIZE TEXT-COLOR)
                    (text "BetterNow 2" TEXT-SIZE TEXT-COLOR)
                    (text "8teen 3" TEXT-SIZE TEXT-COLOR)))
                    
(define (history->images history)
  (local [;history->los : History -> [List-of String]
          ; converts a History into a list of strings
          ; given HISTORY-0, return (list )
          ; given HISTORY-1, return (list "BankAccount 1")
          ; given HISTORY-2, return (cons "BankAccount 1" (cons "BetterNow 2" '()))
          (define (history->los history-other)
            (local [; textify : SongHistory -> String 
                    ; turns a song history into a string
                    ; given SONGHISTORY-1, return "BankAccount 1"
                    ; given SONGHISTORY-2, return "BetterNow 2"
                    ; given SONGHISTORY-3, return "8teen 3"
                    (define (textify sh)
                      (string-append (song-title (songhistory-song sh)) " "
                                     (number->string (songhistory-timesplayed sh))))]
              (if (empty? history-other)
                  (list "")
                  (map textify history-other))))
          ; creates a constant that turns a history into a list of strings where eachs string is the
          ; title of a song and times played of a song history
          (define LIST-OF-HISTORY-AS-STRINGS (history->los history))          
          ; text-function : String -> Image
          ; turns the string into an image with TEXT-SIZE and TEXT-COLOR
          ; given "hello", return (text "hello" TEXT-SIZE TEXT-COLOR)
          (define (text-function s)
            (cond
              [(string=? s "") empty-image]
              [else (text s TEXT-SIZE TEXT-COLOR)]))]
    (map text-function LIST-OF-HISTORY-AS-STRINGS)))



;; save-history : String History -> String
;; Write the list of numbers to the file
(define (save-history fpath history)
  (write-file fpath (history->string history)))

; history->string : History -> String
; converts the History to a storable CSV string
(check-expect (history->string HISTORY-0) "")
(check-expect (history->string HISTORY-1) "2929,BankAccount,1\n")
(check-expect (history->string HISTORY-2) "8573,BetterNow,2\n2929,BankAccount,1\n")
(check-expect (history->string HISTORY-3) "7163,8teen,3\n8573,BetterNow,2\n2929,BankAccount,1\n")

(define (history->string history)
  (local [; sh-smasher : SongHistory String
          ; smashes the song history string and a string together
          ; given SONGHISTORY-1 and " hi", return "BankAccount 1 hi"
          (define (sh-smasher sh string)
            (string-append (songhistory->string sh) string))]
    (foldl sh-smasher "" history)))

; songhistory->string : SongHistory -> String
; converts a single songhistory to a storable CSV string
(check-expect (songhistory->string SONGHISTORY-1) "2929,BankAccount,1\n")
(check-expect (songhistory->string SONGHISTORY-2) "8573,BetterNow,2\n")
(check-expect (songhistory->string SONGHISTORY-3) "7163,8teen,3\n")
              
(define (songhistory->string sh)
  (string-append (number->string (song-id (songhistory-song sh))) ","
                 (song-title (songhistory-song sh)) ","
                 (number->string (songhistory-timesplayed sh)) "\n"))

      







