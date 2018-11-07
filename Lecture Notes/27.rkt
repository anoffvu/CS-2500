;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture27) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANSWERS TO QUESTIONS
;; 1. There are two ways to use stop-when. You can look at the documentation to see
;;    how to pass it an image-drawing function in addition to the predicate you
;;    would usually give.
;; 2. If you need to connect to a server running the protocol from assignment 13, you
;;    can use port 10001 on joshua.ccs.neu.edu

;; Let's take a look at our code from last lecture.

(define-struct webpage [name links])
;; A WebPage is a (make-webpage String [List-of String])
;; - where name is the page's title
;; - and links is the list of links on the page (as represented by those pages' titles)

(define PAGE1 (make-webpage "A" (list "B")))
(define PAGE2 (make-webpage "B" '()))
(define PAGE3 (make-webpage "C" (list "A")))
(define PAGE4 (make-webpage "D" (list "E")))
(define PAGE5 (make-webpage "E" (list "D" "F")))
(define PAGE6 (make-webpage "F" (list "G")))
(define PAGE7 (make-webpage "G" '()))

;; A Wiki is a [List-of WebPage]

(define WIKI0 '())
(define WIKI1 (list PAGE1 PAGE2 PAGE3))
(define WIKI2 (list PAGE4 PAGE5 PAGE6 PAGE7))

;; connected? : Wiki String String -> Boolean
;; Is there a path from p1 to p2 in the given Wiki?
(check-expect (connected? WIKI0 "A" "B") #false)
(check-expect (connected? WIKI1 "C" "B") #true)
(check-expect (connected? WIKI1 "C" "E") #false)
(define (connected? w p1 p2)
  (local [(define LINKS-FROM-START (get-page-links w p1))
          (define IS-END-IN-START-LINKS (member? p2 LINKS-FROM-START))]
    (if IS-END-IN-START-LINKS
        #true
        (any-connected? w LINKS-FROM-START p2))))

;; get-page-links : Wiki String -> [List-of String]
;; Get all the links for the page with the given name
;; Returns an empty list if the page is not found
(check-expect (get-page-links WIKI0 "A") '())
(check-expect (get-page-links WIKI1 "A") (list "B"))
(define (get-page-links w pagename)
  (cond [(empty? w) '()]
        [(cons? w)
         (if (same-name? (first w) pagename)
             (webpage-links (first w))
             (get-page-links (rest w) pagename))]))

;; same-name? : Webpage String -> Boolean
;; Does the given page have the given name?
(check-expect (same-name? PAGE1 "A") #true)
(check-expect (same-name? PAGE2 "C") #false)
(define (same-name? wp wanted)
  (string=? (webpage-name wp) wanted))

;; any-connected? : Wiki [List-of String] String -> Boolean
;; Is there a path from any of the given pages to dest?
(check-expect (any-connected? WIKI0 (list "A" "B" "C") "D") #false)
(check-expect (any-connected? WIKI1 (list "B" "C") "A") #true)
(define (any-connected? w pagelist dest)
  (local [;; String -> Boolean
          ;; Is there a path from p to dest?
          (define (is-path? p)
            (connected? w p dest))]
    (ormap is-path? pagelist)))

;; BECCA : Again, I have put some functions into helpers where Professor Mislove did not but
;; this is just a design choice. I prefer to be able to test these functions and I think it's
;; more consistent. But of course, in the end, the design of your program is up to you, as long
;; as it's clean, readable, and follows the design recipe.

;; What if we call (connected? WIKI2 "D" "G")? It just keeps running and running and running...
;; Why is that? Basically we keep moving back and forth between D and E forever. We check whether
;; D is connected to G. It is not. We check whether its neighbors are connected (so E). E is not
;; connected. Well, what's the first neighbor of E? It is D so now we need to check whether D is
;; connected to G. But that's the thing we were doing before! So now we are just going to check
;; between those two pages forever and ever and ever and ever and ever...

;; Why is it that this is the first time we have encountered such an issue? We've been doing
;; recursion for AGES now and we've never run into this problem before! In the past when we did
;; recursion we had two cases: a base case, and a case where we made the data smaller to get
;; closer to the base case. In the function above we are NOT making the data smaller. We are just
;; inputting DIFFERENT data. So there is no guarantee for us that our code will ever terminate.

;; BECCA : See what happens when you don't follow a template?

;; How can we fix this problem? We want to make the Wiki smaller every time we recur. How can we do
;; that? We can remove a page. What page should we remove? The page we just visited. There's no
;; point in ever going back to that page so there should be no problem removing it.

;; connected?v2 : Wiki String String -> Boolean
;; Is there a path from p1 to p2 in the given Wiki?
(check-expect (connected?v2 WIKI0 "A" "B") #false)
(check-expect (connected?v2 WIKI1 "C" "B") #true)
(check-expect (connected?v2 WIKI1 "C" "E") #false)
(check-expect (connected?v2 WIKI2 "D" "G") #true)
(define (connected?v2 w p1 p2)
  (local [(define LINKS-FROM-START (get-page-links w p1))
          (define IS-END-IN-START-LINKS (member? p2 LINKS-FROM-START))]
    (if IS-END-IN-START-LINKS
        #true
        (any-connected?v2 (remove-page w p1) LINKS-FROM-START p2))))

;; any-connected?v2 : Wiki [List-of String] String -> Boolean
;; Is there a path from any of the given pages to dest?
(check-expect (any-connected?v2 WIKI0 (list "A" "B" "C") "D") #false)
(check-expect (any-connected?v2 WIKI1 (list "B" "C") "A") #true)
(define (any-connected?v2 w pagelist dest)
  (local [;; String -> Boolean
          ;; Is there a path from p to dest?
          (define (is-path? p)
            (connected?v2 w p dest))]
    (ormap is-path? pagelist)))

;; remove-page : Wiki String -> Wiki
;; Remove the page with the given name
(check-expect (remove-page WIKI0 "A") WIKI0)
(check-expect (remove-page WIKI1 "A") (list PAGE2 PAGE3))
(define (remove-page w pagename)
  (cond [(empty? w) '()]
        [(cons? w)
         (if (same-name? (first w) pagename)
             (rest w)
             (cons (first w) (remove-page (rest w) pagename)))]))

;; Note that you can use a filter here but it will recur through more of the list
;; than necessary. Since this course is not all that concerned with efficiency it is fine
;; to use that if you think it is cleaner and more readable. Here's the filter way:

;; remove-page.v2 : Wiki String -> Wiki
;; Remove the page with the given name
(check-expect (remove-page.v2 WIKI0 "A") WIKI0)
(check-expect (remove-page.v2 WIKI1 "A") (list PAGE2 PAGE3))
(define (remove-page.v2 w pagename)
  (local [;; Webpage -> Boolean
          ;; Does this webpage have a DIFFERENT name than pagename?
          (define (diff-name? wp)
            (not (same-name? wp pagename)))]
    (filter diff-name? w)))

;; Design the function any-broken-links? that accepts a Wiki and returns whether or not there are
;; any links to pages that don't exist in the Wiki

;; any-broken-links? : Wiki -> Boolean
;; Are there any broken links in the given Wiki?
(check-expect (any-broken-links? WIKI0) #false)
(check-expect (any-broken-links? (list PAGE1 PAGE2 PAGE3 PAGE4)) #true)
(define (any-broken-links? w)
  (local [;; Webpage -> Boolean
          ;; Does this webpage have a broken link?
          (define (broken-link? wp)
            (any-not-in-wiki? w (webpage-links wp)))]
    (ormap broken-link? w)))

;; any-not-in-wiki? : Wiki [List-of String] -> Boolean
;; Do any of the page names not exist in the Wiki?
(check-expect (any-not-in-wiki? WIKI0 (list "A" "B" "C")) #true)
(check-expect (any-not-in-wiki? WIKI1 (list "A" "B" "C")) #false)
(define (any-not-in-wiki? w pagelist)
  (local [;; String -> Boolean
          ;; Is there NOT a page with the given name in the wiki?
          (define (not-exists? pagename)
            (not (in-wiki? w pagename)))]
    (ormap not-exists? pagelist)))

;; in-wiki? : Wiki String -> Boolean
;; Is there a page with the given name in the given Wiki?
(check-expect (in-wiki? WIKI0 "A") #false)
(check-expect (in-wiki? WIKI1 "B") #true)
(define (in-wiki? w pagename)
  (local [;; Webpage -> Boolean
          ;; Does this webpage have the given name?
          (define (page-match? wp)
            (same-name? wp pagename))]
    (ormap page-match? w)))

;; BECCA : Again, Professor Mislove handled which functions were in the local
;; differently. Basically whenever I saw that I was going to need a list abstraction
;; with a local helper I made that its own function since it seemed sufficiently
;; complex. Again, it's just a design decision. It's up to you to determine which
;; functions can be in a local and which cannot.