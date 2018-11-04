;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture24) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS:
;; 1. You should open DrRacket just to see the fun Halloween logo today!
;; 2. Homework is due tomorrow at 9pm and is another part of the project.

;; Today we're going to talk about dealing with multiple complex inputs.

;; Design the function append-lists that accepts two [List-of Number] and
;; returns a list containing all of the elements of the first followed by all of the elements of
;; the second.

;; append-lists : [List-of Number] [List-of Number] -> [List-of Number]
;; Returns a list containing all of the elements of the first followed by the elements of the 2nd
(check-expect (append-lists '() '()) '())
(check-expect (append-lists '() (list 1 2 3)) (list 1 2 3))
(check-expect (append-lists (list 4 5 6) '()) (list 4 5 6))
(check-expect (append-lists (list 1 2) (list 3 4)) (list 1 2 3 4))
(define (append-lists lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(cons? lon1)
         (cons (first lon1) (append-lists (rest lon1) lon2))]))

;; Notice how above we basically ignore the second input and just work through the template using
;; the first input instead.

;; Consider the following data definition and templates:

;; A Nat is one of:
;; - 0
;; - (add1 Nat)

;; nat-temp : Nat -> ???
(define (nat-temp n)
  (cond [(zero? n) ...]
        [(positive? n) (... (nat-temp (sub1 n)) ...)]))

;; los-temp : [List-of String] -> ???
(define (los-temp los)
  (cond [(empty? los) ...]
        [(cons? los) (... (first los) ... (los-temp (rest los)) ...)]))

;; Design the function get-element that accepts a [List-of String] and a Nat and
;; returns the element at the location in the list specified by the Nat. If there is
;; no such element, it should return #false.

;; A StringOrFalse is one of:
;; - String
;; - #false
(define SOF1 "howdy")
(define SOF2 #false)

;; sof-temp : StringOrFalse -> ???
(define (sof-temp sof)
  (cond [(string? sof) ...]
        [(false? sof) ...]))

;; get-element : [List-of String] Nat -> StringOrFalse
;; Get the element at the given index or return #false if there is no such element


;make a table
;make a super template


(check-expect (get-element '() 0) #false)
(check-expect (get-element '() 10) #false)
(check-expect (get-element (list 1 2 3) 1) 2)
(check-expect (get-element (list 1 2 3) 5) #false)
(define (get-element los n)
  (cond [(and (empty? los) (zero? n)) #false]
        [(and (empty? los) (positive? n)) #false]
        [(and (cons? los) (zero? n)) (first los)]
        [(and (cons? los) (positive? n))
         (get-element (rest los) (sub1 n))]))

;; Here we have merged the templates for a list of strings and the template for a natural
;; number. That's because, unlike in the first case we aren't doing something to one input
;; and THEN dealing with the other input. We have to deal with both inputs at the same
;; time.

;; Notice how both empty cases are #false. It seems like a good idea to simplify.

;; get-element.v2 : [List-of String] Nat -> StringOrFalse
;; Get the element at the given index or return #false if there is no such element
(check-expect (get-element.v2 '() 0) #false)
(check-expect (get-element.v2 (list 1 2 3) 1) 2)
(check-expect (get-element.v2 (list 1 2 3) 5) #false)
(define (get-element.v2 los n)
  (cond [(empty? los) #false]
        [(and (cons? los) (zero? n)) (first los)]
        [(and (cons? los) (positive? n))
         (get-element.v2 (rest los) (sub1 n))]))

;; Note that we don't START with this. We ALWAYS start with the template. If, after you use the
;; template you see a simplification, you can make that change.
;; You could also remove the cons? tests but I think leaving them in for readability is a good idea.

;; So now we have seen 2 cases for multiple complex inputs:
;; 1. We can deal with the inputs one at a time, in which case we just use each template
;;    individually to process them.
;; 2. We have to deal with both inputs simultaneously, in which case we create a combined template
;;    to process them.

;; Design the function intersect that takes two lists of strings and returns those elements of the
;; first that appear in the second (in the order they appear in the first)

; intersect : [List-of String] [List-of String] -> [List-of String]
#; ;my version
(define (intersect los1 los2)
  (local [; String -> Boolean
          (define (in-l2? s)
            (local [; String -> Boolean
                    (define (equal-to-s? s2)
                      (string=? s2 s))]
              (ormap equal-to-s? l2)))]
    (filter in-l2? l1)))



;; intersect : [List-of String] [List-of String] -> [List-of String]
;; Returns the elements in the first list that are also in the second
(check-expect (intersect '() '()) '())
(check-expect (intersect '() (list "a" "b" "c")) '())
(check-expect (intersect (list "a" "b" "c") '()) '())
(check-expect (intersect (list "a" "b" "c" "b" "a") (list "a" "d" "g" "b")) (list "a" "b" "b" "a"))
(define (intersect los1 los2)
  (cond [(empty? los1) '()]
        [(cons? los1)
         (if (element-in-list? (first los1) los2)
             (cons (first los1) (intersect (rest los1) los2))
             (intersect (rest los1) los2))]))

;; element-in-list? : String [List-of String] -> Boolean
;; Is the given element in the given list?
(check-expect (element-in-list? "a" '()) #false)
(check-expect (element-in-list? "b" (list "a" "b" "c")) #true)
(define (element-in-list? s los)
  (cond [(empty? los) #false]
        [(cons? los) (or (string=? s (first los))
                         (element-in-list? s (rest los)))]))

;; Notice how we are working through one list but we need the CONTEXT from another list. What does
;; that remind you of? When we are using LOCAL we are relying on some CONTEXT from our inputs. In
;; addition that first function looks a heck of a lot like a filter. Let's refine.

;; intersect.v2 : [List-of String] [List-of String] -> [List-of String]
;; Returns the elements in the first list that are also in the second
(check-expect (intersect.v2 '() '()) '())
(check-expect (intersect.v2 '() (list "a" "b" "c")) '())
(check-expect (intersect.v2 (list "a" "b" "c") '()) '())
(check-expect
 (intersect.v2 (list "a" "b" "c" "b" "a") (list "a" "d" "g" "b"))
 (list "a" "b" "b" "a"))
(define (intersect.v2 los1 los2)
  (local [;; in-los2? : String -> Boolean
          ;; Is the given string in los2?
          (define (in-los2? s)
            (element-in-list? s los2))]
    (filter in-los2? los1)))

;; That looks much nicer but element-in-list? looks a heck of a lot like an ormap...

;; intersect.v3 : [List-of String] [List-of String] -> [List-of String]
;; Returns the elements in the first list that are also in the second
(check-expect (intersect.v3 '() '()) '())
(check-expect (intersect.v3 '() (list "a" "b" "c")) '())
(check-expect (intersect.v3 (list "a" "b" "c") '()) '())
(check-expect
 (intersect.v3 (list "a" "b" "c" "b" "a") (list "a" "d" "g" "b"))
 (list "a" "b" "b" "a"))
(define (intersect.v3 los1 los2)
  (local [;; in-los2? : String -> Boolean
          ;; Is the given string in los2?
          (define (in-los2? s)
            (local [;; same-string? : String -> Boolean
                    ;; Is the given string the same as s?
                    (define (same-string? s2) (string=? s2 s))]
              (ormap same-string? los2)))]
    (filter in-los2? los1)))

;; So now we have 3 cases:
;; 1. Deal with one input THEN deal with another (use 2 templates)
;; 2. Deal with both inputs simultaneously (use combined template)
;; 3. Deal with one input using CONTEXT from another (use local)