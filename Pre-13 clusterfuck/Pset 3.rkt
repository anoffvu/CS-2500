;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pset 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Data Definitions

; A CryptoName is one of:
; - "Alice"
; - "Bob"
; - "Carol"
; and represents the collection of names used in crypotography narratives

; on-key handlers

; Exercise 1
; Examples:
(define CRYPTO-NAME-ALICE "Alice")
(define CRYPTO-NAME-BOB "Bob")
(define CRYPTO-NAME-CAROL "Carol")

; Exercise 2
#;
(define (crypto-name-temp cn)
  (cond
    [(string=? CRYPTO-NAME-ALICE)...]
    [(string=? CRYPTO-NAME-BOB)...]
    [(string=? CRYPTO-NAME-CAROL)...]))

; Exercise 3

; crypto-name-indentifier : CryptoName KeyEvent -> CryptoName
; given a key, returns a letter or the backup if the key is invalid

(check-expect (crypto-name-identifier "Bob" "a") "Alice")
(check-expect (crypto-name-identifier "Alice" "b") "Bob")
(check-expect (crypto-name-identifier "Bob" "q") "Bob")
(check-expect (crypto-name-identifier "Bob" "c") "Carol")
(check-expect (crypto-name-identifier "Bob" "c") "Carol")

(define (crypto-name-identifier backup ke)
  (cond
    [(key=? ke "a") CRYPTO-NAME-ALICE]
    [(key=? ke "b") CRYPTO-NAME-BOB]
    [(key=? ke "c") CRYPTO-NAME-CAROL]
    [else backup]))

; Exercise 4

; crypto-cycle : CryptoName KeyEvent -> CryptoName
; cycles through the list of a crypto name

(check-expect (crypto-cycle "Bob" "a") "Alice")
(check-expect (crypto-cycle "Alice" "b") "Bob")
(check-expect (crypto-cycle "Bob" "q") "Bob")
(check-expect (crypto-cycle "Bob" "c") "Carol")
(check-expect (crypto-cycle "Bob" "c") "Carol")
(check-expect (crypto-cycle "Bob" "right") "Carol")
(check-expect (crypto-cycle "Alice" "right") "Bob")
(check-expect (crypto-cycle "Carol" "right") "Alice")
    

(define (crypto-cycle cn ke)
  (cond
    [(key=? ke "a") CRYPTO-NAME-ALICE]
    [(key=? ke "b") CRYPTO-NAME-BOB]
    [(key=? ke "c") CRYPTO-NAME-CAROL]
    [(key=? ke "right") (crypto-cycle-handler cn)]
    [else cn]))

; crypto-cycle-handler : CryptoName -> CryptoName
; given a CryptoName, cycles to the next CryptoName
(define (crypto-cycle-handler firstCN)
  (cond
    [(string=? firstCN CRYPTO-NAME-ALICE) CRYPTO-NAME-BOB]
    [(string=? firstCN CRYPTO-NAME-BOB) CRYPTO-NAME-CAROL]
    [(string=? firstCN CRYPTO-NAME-CAROL) CRYPTO-NAME-ALICE]))

; to-draw handlers

; Exercise 5
; draw-crypto : CryptoName -> Image
; given a CryptoName, draws it on an empty scene

; constants

(define FONT-SIZE 24)
(define FONT-COLOR "olive")
(define HEIGHT 150)
(define WIDTH 200)
(define BACKGROUND (empty-scene WIDTH HEIGHT))


(check-expect (draw-crypto CRYPTO-NAME-ALICE)
              (overlay (text CRYPTO-NAME-ALICE FONT-SIZE FONT-COLOR) BACKGROUND))
(check-expect (draw-crypto CRYPTO-NAME-BOB)
              (overlay (text CRYPTO-NAME-BOB FONT-SIZE FONT-COLOR) BACKGROUND))
(check-expect (draw-crypto CRYPTO-NAME-CAROL)
              (overlay (text CRYPTO-NAME-CAROL FONT-SIZE FONT-COLOR) BACKGROUND))

(define (draw-crypto cn)
  (overlay (text cn FONT-SIZE FONT-COLOR) BACKGROUND))

; Exercise 6

; main/crypto : CryptoName -> CryptoName
; draws a CryptoName based on what letter is pressed; allows you to cycle through all CryptoNames

(define (main/crypto initialworld)
  (big-bang initialworld
    [to-draw draw-crypto]
    [on-key crypto-cycle]))
