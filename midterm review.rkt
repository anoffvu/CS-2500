;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |midterm review|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A PB is one of:
; -- '()
; -- (cons String (cons Number PB))
; interpretation A phone book such as
; (cons "Alan" (cons 617738.1212 pb))
; means "Alan"'s phone number is 617738.1212,
; and the rest of the phone book is pb

(define PB-1 (cons "An" (cons 316.6509552 (cons "Rizzy" (cons 301.7062804 '())))))

(check-expect (remove-both "An" PB-1) (cons "Rizzy" (cons 301.7062804 '())))

(define (remove-both n pb)
  (cond
    [(empty? pb) '()]
    [(cons? pb) (if (string=? (first pb) n)
                    (remove-both n (rest (rest pb)))
                    (cons (first pb) (cons (first (rest pb)) (remove-both n (rest (rest pb))))))]))

(define LOP-1 (list (make-posn 0 0) (make-posn 200 201) (make-posn 100 100)))
(define LOP-2 (list (make-posn 0 0) (make-posn 100 100)))

(check-expect (drop LOP-1) LOP-2)

(define (drop lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop) (if (> (posn-y (first lop)) 200)
                     (drop (rest lop))
                     (cons (first lop) (drop (rest lop))))]))
                     