;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A GameOfLife is a [List-of [List-of Boolean]]
; where the length of the outer list is the same as all of the inner lists
; and represents alive (#t) and dead (#f) cells
(define TINY-GAME
  (list (list #f #f #f #f)
        (list #f #t #t #f)
        (list #f #t #f #f)
        (list #f #f #f #f)))

;ex 1
(define NEXT-TINY-GAME
  (list (list #f #f #f #f)
        (list #f #t #t #f)
        (list #f #t #t #f)
        (list #f #f #f #f)))

;ex 2
; neighbors : Nat Nat GameOfLife -> [List-of Boolean]
; The neighbors of i, j in gol (i = row, j = column)
(check-expect (neighbors 1 1 TINY-GAME)
              (list #f #f #f
                    #f    #t
                    #f #t #f))
(check-expect (neighbors 3 1 TINY-GAME)
              (list #f #t #f
                    #f    #f
                    #f #f #f))
(define (neighbors i j gol)
  (list (get-cell (sub1 i) (sub1 j) gol)
        (get-cell (sub1 i) j        gol)
        (get-cell (sub1 i) (add1 j) gol)
        (get-cell i        (sub1 j) gol)
        (get-cell i        (add1 j) gol)
        (get-cell (add1 i) (sub1 j) gol)
        (get-cell (add1 i) j        gol)
        (get-cell (add1 i) (add1 j) gol)))
 
; get-cell : Nat Nat GameOfLife -> Boolean
; The value of the cell at y, x (looping around the ends of the list if necessary)
(check-expect (get-cell 0 0 TINY-GAME) #f)
(check-expect (get-cell 4 4 TINY-GAME) #f)
(check-expect (get-cell 1 2 TINY-GAME) #t)
(check-expect (get-cell 5 6 TINY-GAME) #t)
(define (get-cell y x gol)
  (list-ref (list-ref gol (modulo y (length gol)))
            (modulo x (length gol))))

; number-of-trues : [List-of Boolean] -> Number
; counts how many trues are in a list of booleans
(check-expect (number-of-trues (list #true #true #true)) 3)
(check-expect (number-of-trues (list #true #true #false)) 2)

(define (number-of-trues lob)
  (length (filter identity lob)))

; new-value/conway : Boolean [List-of Boolean] -> Boolean
; updates a cell based on the values of its neighbors
(check-expect (new-value/conway #t (list #f #f #true #false #false #false #false #false))
              #f)
(check-expect (new-value/conway #t (list #true #true #true #t #false #false #false #false))
              #f)
(check-expect (new-value/conway #true (list #true #true #true #false #false #false #false #false))
              #true)
(check-expect (new-value/conway #false (list #true #true #true #false #false #false #false #false))
              #true)

(define (new-value/conway cell-current lon)
  (local [(define NUM-TRUE (number-of-trues lon))]
    (cond
      [cell-current (cond
                        [(< NUM-TRUE 2) #false]
                        [(> NUM-TRUE 3) #false]
                        [else #true])]
      [(not cell-current) (= NUM-TRUE 3)])))

; next-grid : GOL -> GOL
; advances the game one step

(define (next-grid gol)
  (local [()]
  (build-list (length gol) build-horiz-list))




