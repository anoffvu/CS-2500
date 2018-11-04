;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture23) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS
;; 1. There has been a rise in repeated questions on Piazza. Please remember that it takes time
;;    and effort for the staff to answer your questions. Before asking your question please use
;;    the search feature on Piazza and/or look through the tag for that assignment to make sure
;;    your question has not already been answered. If you have a question that is similar but
;;    not quite the same please use the followup feature rather than posting a whole new question.

;; Design a data definition for a person, who has two biological parents.

(define-struct person [name p1 p2])

;; A Person is a (make-person String Person Person)
;; - where name is the person's name
;; - p1 is the person's first parent
;; - and p2 is the person's second parent

;; Let's try to make an example of a Person. We get an issue where we can't create a Person because
;; we need to create a Person first before we can create a Person. It's the "chicken and the egg"
;; problem!

;; Let's think about our family tree. Maybe you know the names of your parents, and your
;; grandparents but not your great-grandparents. Or maybe you know your great-grandparents
;; but not your great-great-grandparents. That is, at some point, you stop knowing the names of
;; your ancestors. Let's make this part of our data definition

;; A Person is one of:
;; - #false (representing an unknown person)
;; - (make-person String Person Person)
;;   - where name is the person's name
;;   - p1 is the person's first parent
;;   - and p2 is the person's second parent

;; This is similar to a list in that it's a recursive data definition, but it's different in that
;; it has two separate "branches" of the definition. You can either go looking through the person's
;; ancestors via parent 1 or you can go looking through the person's ancestors via parent 2.
;; This kind of data is called a "tree" and is VERY common in computer science.

(define PERSON0 #false)
(define PERSON1 (make-person "Albert" PERSON0 PERSON0))
(define PERSON2 (make-person "Bethany" PERSON0 PERSON0))
(define PERSON3 (make-person "Leo" PERSON1 PERSON2))
(define PERSON4 (make-person "Carlie" PERSON3 PERSON0))

;; person-temp : Person -> ???
(define (person-temp p)
  (cond [(false? p) ...]
        [(person? p)
         (... (person-name p)
              (person-temp (person-p1 p))
              (person-temp (person-p2 p)) ...)]))

;; Design the function tree-size which takes in a Person and returns the number of named people in
;; that person's family tree.

;; tree-size : Person -> Nat
;; Returns the number of named people in this person's family tree
(check-expect (tree-size PERSON0) 0)
(check-expect (tree-size PERSON4) 4)
(define (tree-size p)
  (cond [(false? p) 0]
        [(person? p)
         (+ 1 (tree-size (person-p1 p))
            (tree-size (person-p2 p)))]))

;; Design the function tree-depth which takes in a Person and returns the number of "generations" in
;; that person's family tree.

;; tree-depth : Person -> Nat
;; Returns the number of generations of known people in this person's family tree
(check-expect (tree-depth PERSON0) 0)
(check-expect (tree-depth PERSON4) 3)
(define (tree-depth p)
  (cond [(false? p) 0]
        [(person? p)
         (add1 (max (tree-depth (person-p1 p))
                    (tree-depth (person-p2 p))))]))

;; The data definition we made above is called a "binary tree" because it has TWO branches. We can
;; also make a "ternary tree" which has THREE branches.

(define-struct ternary [val left middle right])

;; A [TernaryTree X] is one of:
;; - #false
;; - (make-ternary X [TernaryTree X] [TernaryTree X] [TernaryTree X])

(define TT0 #false)
(define TT1 (make-ternary "hello" TT0 TT0 TT0))
(define TT2 (make-ternary "goodbye" TT0 TT0 TT0))
(define TT3 (make-ternary "ciao" TT1 TT0 TT2))

;; tt-temp : TernaryTree -> ???
(define (tt-temp tt)
  (cond [(false? tt) ...]
        [(ternary? tt)
         (... (ternary-val tt)
              (tt-temp (ternary-left tt))
              (tt-temp (ternary-middle tt))
              (tt-temp (ternary-right tt)))]))

;; BECCA : Professor Mislove referred to this as a 3-Tree which also works. They are the same
;; thing, I just prefer to use "ternary" to describe a tree of 3 branches since it lines up more
;; with our use of "binary" to describe a tree of 2 branches.

;; Think of a web page. Webpages can link to other webpages but they can link to more than 2 or
;; 3 pages. In the old days (the early 1990s) there used to be pages that just linked to all the
;; web pages people thought were cool so there would be a page with just a thousand links. How
;; can we represent this structure of the web?

(define-struct webpage [name links])

;; A WebPage is a (make-webpage String [List-of Webpage])
;; - where name is the page's title
;; - and links is the list of links on the page

;; Why is it that we don't need an "is one of" definition here? It's a recursive data definition
;; so don't we need some base value? It turns out that value is built in by using the list as our
;; representation of links. Because a list already has an "is one of" definition we can create
;; web pages without any cyclic issues.

(define WEBPAGE0 (make-webpage "Dead end" '()))
(define WEBPAGE1 (make-webpage "List of dead ends" (list WEBPAGE0)))
(define WEBPAGE2 (make-webpage "My webpage" (list WEBPAGE1)))
(define WEBPAGE3 (make-webpage "Google" (list WEBPAGE1 WEBPAGE2)))

;; Can we make an example of two webpages that link to each other? The answer is NO because in
;; order to define one of these pages we have to define the other. We can't create such a thing.
;; We can't represent cycles in webpage links with our current representation, which turns out
;; to be really great because it makes calculating information about webpages much easier. We'll
;; talk more about cycles in a couple of weeks and then everything will be harder.

;; webpage-temp : WebPage -> ???
(define (webpage-temp wp)
  (... (webpage-name wp) ... (lowp-temp (webpage-links wp)) ...))

;; lowp-temp : [List-of WebPage] -> ???
(define (lowp-temp lowp)
  (cond [(empty? lowp) ...]
        [(cons? lowp)
         (... (webpage-temp (first lowp)) ...
              (lowp-temp (rest lowp)) ...)]))

;; What's weird about this template? Well, webpage-temp calls lowp-temp, which is pretty normal. But
;; then lowp-temp calls webpage-temp again! This is called "MUTUAL RECURSION". Let's give it a try.

;; Design the function 'exists?' which accepts a WebPage and a String and determines whether a page
;; with that title exists.

;; exists? : WebPage String -> Boolean
;; Does a webpage with the given title exist?
(check-expect (exists? WEBPAGE2 "Facebook") #false)
(check-expect (exists? WEBPAGE3 "Dead end") #true)
(define (exists? wp desired-name)
  (or (string=? desired-name (webpage-name wp))
      (any-same-name? (webpage-links wp) desired-name)))

;; any-same-name? : [List-of WebPage] String -> Boolean
;; Do any of these webpages have the given title?
(check-expect (any-same-name? '() "Anything") #false)
(check-expect (any-same-name? (list WEBPAGE0 WEBPAGE1 WEBPAGE3) "My webpage") #true)
(define (any-same-name? lowp desired-name)
  (local [;; WebPage -> Boolean
          ;; Does this webpage or any of its links have the desired title?
          (define (same-name? wp)
            (exists? wp desired-name))]
    (ormap same-name? lowp)))

;; BECCA : Professor Mislove wrote his any-same-name? function with the list template we designed
;; above. However, we later discussed using ormap instead so I just skipped the list template step.
;; At this point in the course you should be able to see when a list abstraction is appropriate.