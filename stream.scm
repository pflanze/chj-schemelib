;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (fixnum inc dec)
	 lazy
	 define-strict-and-lazy
	 cj-struct
	 list-util
	 cj-cmp
	 (cj-functional compose compose*)
	 srfi-11
	 cj-typed
	 cut
	 debuggable-promise
	  ;; tests:
	 (lazy-debug F)
	 show
	 test
	 (cj-env on))

(export stream-null?
	stream-pair?
	stream-filter/tail
	stream-for-each
	stream-for-each/iota
	stream-fold-right
	stream:fold-right
	stream-map/tail
	stream-map1
	stream-map
	stream-map-list
	stream-filter-map/tail
	filter-map/iota stream-filter-map/iota
	stream-map/iota
	stream-filter/iota
	fold-right/iota stream-fold-right/iota
	fold-right/iota+rest stream-fold-right/iota+rest
	stream-filter-map
	stream-improper-map
	stream->list
	stream->string
	source-stream->string
	stream-drop
	stream-take
	stream-last
	stream-butlast
	list-rtake&rest stream-rtake&rest
	reverse/tail stream-reverse/tail
	stream-reverse
	list-split-at stream-split-at
	stream-sublist
	stream-length
	stream-length>=
	stream-length>
	(struct difference-at)
	(struct no-difference)
	stream-difference
	show-stream-difference
	stream-equal?
	stream-filter
	stream-fold-left stream-fold
	stream-append-optimized
	list-append-optimized
	stream-append/2
	stream-append
	stream-iota
	;; huh why here? XX vs test-random ?
	random-integer-list
	list-union stream-union
	list-uniq stream-uniq
	list-uniq-count stream-uniq-count
	cmp-list-union cmp-stream-union
	cmp-list-uniq cmp-stream-uniq
	list-group stream-group
	cmp-list-group cmp-stream-group
	chop/map stream-chop/map
	chop stream-chop
	stream-unfold
	stream-unfold2
	stream-zip
	zip2 stream-zip2
	stream-find-tail
	stream-take-while
	stream-drop-while
	
	stream-ref
	;; stream-%cars+cdrs
	stream-every
	gen-infinite-stream
	gen-stream
	stream-min&max
	stream-min
	stream-max
	stream-sum

	stream-first
	stream-second
	stream-third
	stream-fourth
	stream-fifth
	stream-sixth
	stream-seventh
	stream-eighth
	stream-ninth
	stream-tenth)

(possibly-use-debuggable-promise)


(define (stream-null? s)
  (null? (force s)))

(define (stream-pair? s)
  (pair? (force s)))


(define (stream-filter/tail pred s tail)
  (let rec ((s s))
    (delay
      (let ((s (force s)))
	(cond ((pair? s)
	       (let ((a (force (car s))))
		 (If (pred a)
		     (cons a
			   (rec (cdr s)))
		     (rec (cdr s)))))
	      ((null? s)
	       tail)
	      (else
	       (error "improper stream:" s)))))))


(TEST
 > (require (lazy-debug)))
(TEST
 > (F (stream-filter/tail even? (list 1 2 3 4 5) '(a b c)))
 (2 4 a b c)
 > (F (stream-filter/tail even? (list 1) '(a b c)))
 (a b c)
 > (F (stream-filter/tail even? (list) '(a b c)))
 (a b c)
 > (F (stream-filter/tail even? (list 2) '(a b c)))
 (2 a b c)
 )

(define (quickhack-stream-for-each* proc ss)
  ;; allocates pairs for nothing but I don't care right now
  (stream-length (apply stream-map proc ss)))

(define (stream-for-each proc s . ss)
  (if* ss
       (quickhack-stream-for-each* proc (cons s ss))
       (let lp ((s s))
	 (let ((s (force s)))
	   (cond ((null? s)
		  (void))
		 ((pair? s)
		  (proc (force (car s)))
		  (lp (cdr s)))
		 (else
		  (error "improper stream:" s)))))))

(TEST
;  > (stream-for-each pp (Set->stream (Set-intersection (list->Set real-cmp (list 1)) (list->Set real-cmp (list 2)))))
;  > (stream-for-each pp (Set->stream (Set-union (list->Set real-cmp (list 1)) (list->Set real-cmp (list 2)))))
;  1
;  2
)

(define (stream-for-each/iota proc lis)
  (let lp ((lis lis)
	   (i 0))
    (FV (lis)
	(if (null? lis) (void)
	    (let ((a (car lis))
		  (r (cdr lis)))
	      (proc a i)
	      (lp r (inc i)))))))



(define (stream-fold-right kons tail s)
  (let rec ((s s))
    (delay
      (let ((s (force s)))
	(cond ((null? s)
	       tail)
	      ((pair? s)
	       (kons (car s)
		     (rec (cdr s))))
	      (else
	       (error "stream-fold-right: improper stream:" s)))))))

;; variant that doesn't delay evaluation:
;; (ugly copy-paste)
(define (stream:fold-right kons tail s)
  (let rec ((s s))
    (let ((s (force s)))
      (cond ((null? s)
	     tail)
	    ((pair? s)
	     (kons (car s)
		   (rec (cdr s))))
	    (else
	     (error "stream-fold-right: improper stream:" s))))))



;; adapted (list-util-1 map/iota)
(define (stream-map/iota fn lis)
  (let rec ((lis lis)
	    (i 0))
    (delay
      (FV (lis)
	  (if (null? lis) lis
	      (cons (fn (car lis) i)
		    (rec (cdr lis) (inc i))))))))

;; adapted (list-util-1 filter/iota)
(define (stream-filter/iota pred lis)
  (let rec ((lis lis)
	    (i 0))
    (delay
      (FV (lis)
	  (if (null? lis) lis
	      (let ((a (car lis))
		    (r (rec (cdr lis) (inc i))))
		(if (pred (car lis) i)
		    (cons a r)
		    r)))))))

(define-strict-and-lazy
  fold-right/iota
  stream-fold-right/iota
  (lambda (kons/3 tail s)
    (let rec ((s s)
	      (i 0))
      (DELAY
       (let ((s (FORCE s))) ;; force?
	 (cond ((null? s)
		tail)
	       ((pair? s)
		(let-pair ((a r) s)
			  (kons/3 a
				  (rec r
				       (fx+ i 1))
				  i)))
	       (else
		(error "fold-right/iota: improper stream:" s))))))))

(define-strict-and-lazy
  fold-right/iota+rest
  stream-fold-right/iota+rest
  (lambda (kons/3 tail s)
    (let rec ((s s)
	      (i 0))
      (DELAY
       (let ((s (FORCE s))) ;; force?
	 (cond ((null? s)
		tail)
	       ((pair? s)
		(let-pair ((a r) s)
			  (kons/3 a
				  (rec r
				       (fx+ i 1))
				  i
				  r)))
	       (else
		(error "fold-right/iota+rest: improper stream:" s))))))))

(TEST
 > (promise? (stream-fold-right/iota vector '(the rest) '(a b c)))
 #t
 > (fold-right/iota vector '(the rest) '(a b c))
 #(a #(b #(c (the rest) 2) 1) 0)
 > (fold-right/iota+rest vector '(the rest) '(a b c))
 #(a
   #(b
     #(c
       (the rest)
       2
       ())
     1
     (c))
   0
   (b c)))


(define (stream-map/tail func s tail)
  ;; maybe with an adjusted error message?..
  (stream-fold-right (lambda (val tail)
		       (cons (func val)
			     tail))
		     tail
		     s))

(define (stream-map1 func s)
  (stream-map/tail func s '()))

(define (stream-map f s . ss)
  (if (null? ss)
      (stream-map1 f s)
      (let recur ((ss (cons s ss)))
	(delay
	  (receive (cars cdrs) (%cars+cdrs (map force ss))
		   (if (pair? cars)
		       (let ((x (apply f cars))) ; Do head first,
			 (cons x (recur cdrs)))  ; then tail.
		       '()))))))

(TEST
 > (F (stream-map inc-function (list 10 20 30)))
 (11 21 31)
 > (F (stream-map/tail inc-function (list 10 20 30) '(a b)))
 (11 21 31 a b)
 > (F (stream-map/tail inc-function (list) '(a b)))
 (a b)
 > (F (stream-map vector (stream-iota) '(a b c)))
 (#(0 a) #(1 b) #(2 c))
 )

(define (stream-map-list fn s)
  (define tail '())
  (let rec ((s s))
    (FV (s)
	(if (null? s)
	    tail
	    (let-pair ((a s*) s)
		      (cons (fn a)
			    (rec s*)))))))

(TEST
 > (stream-map-list inc-function '(10 12))
 (11 13)
 > (stream-map-list inc-function (stream-take (stream-iota) 3))
 (1 2 3))



;; Also see stream-mapfilter/tail etc. in stream-Maybe.scm !
(define (stream-filter-map/tail func s tail)
  ;; maybe with an adjusted error message?..
  (stream-fold-right (lambda (val tail)
		       (let ((r (func val)))
			 (if r
			     (cons r tail)
			     tail)))
		     tail
		     s))

;; Also see stream-mapfilter etc. in stream-Maybe.scm !
(define-strict-and-lazy
  filter-map/iota
  stream-filter-map/iota
  aliases: ((_fold-right/iota fold-right/iota stream-fold-right/iota))
  (lambda (func s #!key (tail '()))
    ;; maybe with an adjusted error message?..
    (_fold-right/iota (lambda (val tail i)
			(let ((r (func val i)))
			  (if r
			      (cons r tail)
			      tail)))
		      tail
		      s)))

(TEST
 > (promise? (stream-filter-map/iota (lambda (v i) (and (even? i) (inc v))) '(1 2 3 4)))
 #t
 > (F (stream-filter-map/iota (lambda (v i) (and (even? i) (inc v))) '(1 2 3 4)))
 (2 4)
 > (filter-map/iota (lambda (v i) (and (even? i) (inc v))) '(1 a 3 b))
 (2 4))


;; only for 1 argument for now

;; Also see stream-mapfilter etc. in stream-Maybe.scm !
(define (stream-filter-map f s . ss)
  (if (null? ss)
      (stream-filter-map/tail f s '())
      (error "stream-filter-map with multiple arguments not implemented yet")))


(define (stream-improper-map func s)
  (let rec ((s s))
    (delay
      (let ((v (force s)))
	(cond ((null? v)
	       v)
	      ((pair? v)
	       (cons (func (car v))
		     (rec (cdr v))))
	      (else
	       (func v)))))))

(define (stream->list s)
  (let rec ((s s))
    (let ((p (force s)))
      (cond ((null? p)
	     '())
	    ((pair? p)
	     (cons (car p)
		   (rec (cdr p))))
	    (else
	     ;; (don't keep a reference to the stream head to avoid
	     ;; memory retention!)
	     (error "stream->list: improper stream, ending in:" p))))))


(define (stream->string s)
  (let* ((len (stream-length s))
	 (str (make-string len)))
    (let lp ((s s)
	     (i 0))
      (FV (s)
	  (if (fx< i len)
	      (let-pair ((a r) s)
			(string-set! str i a)
			(lp r (fx+ i 1))))))
    str))

;; copy-paste of stream->string with the addition of source-code
;; handling
(define (source-stream->string s #!optional track-source?)
  (let* ((len (stream-length s))
	 (str (make-string len)))
    (let lp ((s s)
	     (i 0))
      (FV (s)
	  (if (fx< i len)
	      (let-pair ((a r) s)
			(string-set! str i (source-code a))
			(lp r (fx+ i 1))))))
    (if (and track-source?
	     (fx> len 0))
	(possibly-sourcify str (car (force s)))
	str)))


(define (stream-drop s k)
  (if (>= k 0)
      (let iter ((s s)
		 (k k))
	(if (zero? k) s
	    (let ((p (force s)))
	      (if (pair? p)
		  (iter (cdr p)
			(dec k))
		  (error "stream-drop: stream too short")))))
      (error "stream-drop: negative k:" k)))

(define (stream-take s k #!optional (tail '()))
  (if (>= k 0)
      (let rec ((s s)
		(k k))
	;;(delay/trace "stream-take"
	(delay
	  (if (zero? k)
	      tail
	      (let ((p (force s)))
		(cond ((pair? p)
		       (cons (car p)
			     (rec (cdr p)
				  (dec k))))
		      ((null? p)
		       p)
		      (error "stream-take: improper stream:" p))))))
      (error "stream-take: negative k:" k)))


(define (stream-last s)
  (FV (s)
      (let-pair ((a s*) s)
		(FV (s*)
		    (if (null? s*)
			a
			(stream-last s*))))))

(define (stream-butlast s #!optional (tail '()))
  (let rec ((s s))
    (delay
      (FV (s)
	  (let-pair ((a s*) s)
		    (FV (s*)
			(if (null? s*)
			    tail
			    (cons a (rec s*)))))))))


(TEST
 > (stream-last (.stream "Hello"))
 #\o
 > (stream-last (.stream "a"))
 #\a
 > (%try (stream-last (.stream "")))
 (exception text: "(Argument 1) PAIR expected\n(car '())\n")
 > (F (stream-butlast (.stream "Hello")))
 (#\H #\e #\l #\l)
 > (promise? (stream-butlast (.stream "")))
 #t
 > (%try (force (stream-butlast (.stream ""))))
 (exception text: "(Argument 1) PAIR expected\n(car '())\n"))


;; combined stream-take and stream-drop, but eager, returning the take
;; in reverse order
(define-strict-and-lazy
  list-rtake&rest
  stream-rtake&rest
  (lambda (s n #!optional (tail '()))
    (let lp ((s s)
	     (res tail)
	     (n n))
      (cond ((zero? n)
	     (values res s))
	    ((negative? n) ;; yes  "should-could" be moved out to the outer scope
	     (error "negative n:" n))
	    (else
	     (FV (s)
		 (lp (cdr s)
		     (cons (car s) res)
		     (dec n))))))))
;; (tests see test-lib.scm)


(define-strict-and-lazy
  reverse/tail
  stream-reverse/tail
  (named reverse/tail
	 (lambda (s tail)
	   (FV (s)
	       (if (null? s)
		   tail
		   (let-pair ((a r) s)
			     (reverse/tail r
					   (cons a tail))))))))

(TEST
 > (reverse/tail '(a b c) '(1 2))
 (c b a 1 2)
 > (stream-reverse/tail (stream-iota 5) '(1 2))
 (4 3 2 1 0 1 2))

(define (stream-reverse s)
  (stream-reverse/tail s '()))


(define (rtake&->take& f)
  (lambda (s n #!optional (tail '()))
    (letv ((rtak res) (f s n))
	  (values (reverse/tail rtak tail) res))))

;; Don't call it stream-take&rest, srfi-1 has it with a better name,
;; split-at, already; but it doesn't take a tail argument. And reverse
;; might be more efficient than the repeated values (well, just in
;; Gambit, stupid?)
(define list-split-at (rtake&->take& list-rtake&rest))
(define stream-split-at (rtake&->take& stream-rtake&rest))


(define (stream-sublist s si ei)
  (stream->list (stream-take (stream-drop s si) (- ei si))))


(TEST

 > (define s5 (delay (cons 1 (delay (cons 2 (delay (cons 3 (delay (cons 4 (delay (cons 5 '())))))))))))
> (stream->list s5)
(1 2 3 4 5)
> (stream->list (stream-take s5 3))
(1 2 3)
> (stream->list (stream-take s5 0))
()
> (stream->list (stream-take s5 6))
(1 2 3 4 5)
;;^ ok?

> (stream->list (stream-drop s5 5))
()
> (stream->list (stream-drop s5 4))
(5)
> (stream->list (stream-drop s5 3))
(4 5)
> (stream->list (stream-drop s5 0))
(1 2 3 4 5)
; > (stream->list (stream-drop s5 6))
; *** ERROR IN (console)@12.15 -- stream-drop: stream too short

; > (stream-drop s5 3)
; #<promise #3>
; > (stream-drop # 1)
; #<promise #2>
; > (stream-drop # 1)
; ()
; Hmm not a promise, ok?

> (stream-sublist s5 2 2)
()
; > (stream-sublist s5 2 0)
; *** ERROR IN stream-sublist, "../stream.scm"@18.1 -- stream-take: negative k: -2
> (stream-sublist s5 2 3)
(3)
> (stream-sublist s5 2 5)
(3 4 5)
> (stream-sublist s5 2 6)
(3 4 5)
)


(define (stream-length s)
  (let lp ((s s)
	   (n 0))
    (let ((p (force s)))
      (cond ((null? p)
	     n)
	    ((pair? p)
	     (lp (cdr p)
		 (inc n)))
	    (else
	     ;; (don't keep a reference to the stream head to avoid
	     ;; memory retention!)
	     (error "stream->list: improper stream, ending in:" p))))))


;; also see improper-list/length>=
;; (also, strict-and-lazy list-length>=  -- why bother?)
(define (stream-length>= l len)
  (if (positive? len)
      (FV (l)
	  ;; does not report failures hitting improper stream, OK?
	  (and (pair? l)
	       (stream-length>= (cdr l)
				(dec len))))
      #t))

(define (stream-length> l len)
  (stream-length>= l (inc len)))


(define-struct difference-at
  constructor-name: difference-at
  n s1 s2)

(define-struct no-difference
  constructor-name: no-difference
  n)

(define (show-stream-*-difference d #!optional (takelen 2))
  (let* ((sublist+rest
	  (lambda (v)
	    (let* ((start (stream-sublist v 0 takelen))
		   (rest (stream-drop v (length start))))
	      (values start rest))))
	 (show
	  (lambda (v)
	    (letv ((start rest) (sublist+rest v))
		  (if (null? (force rest))
		      start
		      (append start '(...)))))))
    (cond ((no-difference? d)
	   d)
	  (else
	   (let-difference-at
	    ((n s1 s2) d)
	    (list n: n
		  s1: (show s1)
		  s2: (show s2)))))))

(define-strict-and-lazy
  list-difference
  stream-difference
  (lambda (s1 s2 #!optional (equal? equal?))
    (let lp ((n 0)
	     (s1 s1)
	     (s2 s2))
      (FV (s1 s2)
	  (define (differs)
	    (difference-at n s1 s2))
	  (if (null? s1)
	      (if (null? s2)
		  (no-difference n)
		  (differs))
	      (if (null? s2)
		  (differs)
		  (if (equal? (car s1) (car s2))
		      (lp (inc n) (cdr s1) (cdr s2))
		      (differs))))))))

(define (show-stream-difference s1 s2
				#!key
				(equal? equal?)
				(n 2))
  (show-stream-*-difference
   (stream-difference s1 s2 equal?)
   n))

(define (show-list-difference s1 s2
				#!key
				(equal? equal?)
				(n 2))
  (show-stream-*-difference
   (list-difference s1 s2 equal?)
   n))


(define (stream-equal? s1 s2 #!optional (equal? equal?))
  (no-difference? (stream-difference s1 s2 equal?)))

(define (list-equal? s1 s2 #!optional (equal? equal?))
  (no-difference? (list-difference s1 s2 equal?)))

(TEST
 > (stream-equal? '() '())
 #t
 > (stream-equal? '(a) '())
 #f
 > (stream-equal? '(a) '(b))
 #f
 > (stream-equal? '(a) '(a))
 #t
 > (stream-equal? '(a) '(a b))
 #f
 > (stream-equal? '(a b) '(a b))
 #t
 > (list-equal? '(a) '(a b))
 #f
 > (list-equal? '(a b) '(a b))
 #t
 > (def a1 (cons 'a (delay (list))))
 > (def a2 (cons 'a (delay (list 'b))))
 > (def b (cons 'a (delay (list 'b))))
 > (stream-equal? a1 b)
 #f
 > (stream-equal? a2 b)
 #t
 > (with-exception-catcher type-exception? (lambda () (list-equal? a1 b) 'f))
 #t
 > (with-exception-catcher type-exception? (lambda () (list-equal? a2 b) 'f))
 #t
 )


(define (stream-filter pred s #!optional (tail '()))
  (stream-filter/tail pred s tail))

(TEST
 > (stream->list (stream-filter even? '(1 2 3 4 5 6 29 38 36)))
 (2 4 6 38 36)
 > (stream->list (stream-filter odd? '(1 2 3 4 5 6 29 38 36)))
 (1 3 5 29)
 )

(define (stream-fold-left fn z s)
  (let lp ((s s)
	   (z z))
    ;;(delay nope
    (let ((p (force s)))
      (cond ((null? p)
	     z)
	    ((pair? p)
	     (lp (cdr p)
		 (fn (car p)
		     z)))
	    (else
	     (error "stream-fold-left: improper stream, ending in:" p))))))

;; still offer the same, just expecting it some times,  OK?
(define stream-fold stream-fold-left)


(TEST
 > (stream-fold-left vector 0 '(1 2 3))
 #(3 #(2 #(1 0)))
 > (stream-fold-left cons '() '(1 2 3))
 (3 2 1)
;; hm fold right.
; > (stream-fold-right cons '() '(1 2 3))
; #<promise #2>
; > (force #)
; (1 . #<promise #3>)
; > (force #3)
; (2 . #<promise #4>)
; > (force #4)
; (3 . #<promise #5>)
; > (force #5)
; ()
)


;; only copy s1 if s2 is actually something; can't do this by default
;; since it would force early evaluation of the first element of s2
(define (stream-append-optimized s1 s2)
  (if (null? (force s2))
      s1
      (stream-append s1 s2)))

(define (list-append-optimized s1 s2)
  (if (null? (force s2)) ;; force or not here?
      s1
      (append s1 s2)))

(define (stream-append/2 s1 s2)
  (let lp ((s s1))
    (delay
      (let ((p (force s)))
	(cond ((null? p)
	       s2)
	      ((pair? p)
	       (cons (car p)
		     (lp (cdr p))))
	      (else
	       (error "stream-append: improper stream, ending in:" p)))))))

(define (stream-append . ss)
  (delay
   (FV (ss)
       (cond ((null? ss)
	      '())
	     ((null? (cdr ss))
	      (car ss))
	     (else
	      (stream-append/2 (car ss)
			       (apply stream-append (cdr ss))))))))

(TEST
 > (F (stream-append))
 ()
 > (F (stream-append '(a)))
 (a)
 > (F (stream-append '(a) '(b)))
 (a b)
 > (F (stream-append '(a b) '(c) '(d e)))
 (a b c d e)
 ;; are non-copy optimizations correct?
 > (let ((s (stream-iota 5)))
     (eq? (force (stream-append s))
	  (force s)))
 #t
 > (let ((s (stream-iota 5)))
     (eq? (force (stream-drop
		  (stream-append '(a b) s)
		  2))
	  (force s)))
 #t
 > (let ((s (stream-iota 5)))
     (eq? (force (stream-drop
		  (stream-append (stream-iota 2) (stream-iota 3) s)
		  5))
	  (force s)))
 #t)


(define (stream-iota #!optional maybe-n maybe-start maybe-tail)
  (let* ((start (or maybe-start 0))
	 (end (and maybe-n (+ start maybe-n)))
	 (tail (or maybe-tail '())))
    (let rec ((i start))
      (delay (if (or (not end)
		     (< i end))
		 (cons i
		       (rec (inc i)))
		 tail)))))

(TEST
;  > (stream-iota 10)
;  #<promise #2>
;  > (stream->list #)
;  (0 1 2 3 4 5 6 7 8 9)
;  > (stream-iota 10 4)
;  #<promise #3>
;  > (stream->list #)
;  (4 5 6 7 8 9 10 11 12 13)
;  > (stream-iota 10 1)
;  #<promise #4>
;  > (stream->list #)
;  (1 2 3 4 5 6 7 8 9 10)
;  > (stream-iota 1)
;  #<promise #5>
;  > (stream->list #)
;  (0)
;  > (stream-iota 0)
;  #<promise #6>
;  > (stream->list #)
;  ()
)

(define (random-integer-list n range #!optional (lo 0))
  (let* ((get-int (lambda ()
		    (+ lo (random-integer range)))))
    (let lp ((l '())
	     (i n))
      (cond ((<= i 0)
	     l)
	    (else
	     (lp (cons (get-int)
		       l)
		 (dec i)))))))

; (define (cars+cdrs ss ok end)
;   ;; ok receives cars and cdrs lists, in reverse order of ss
;   ;; end receives the lists seen so far (reverse order), and remaining (normal order)
;   (let lp ((ss ss)
; 	   (cars '())
; 	   (cdrs '())
; 	   (seen '()))
;     (if (null? ss)
; 	(ok cars cdrs)
; 	(let ((a (car ss))
; 	      (r (cdr ss)))
; 	  (if (null? a)
; 	      (end seen r)
; 	      (lp r
; 		  (cons (car a)
; 			cars)
; 		  (cons (cdr a)
; 			cdrs)
; 		  (cons a
; 			seen)))))))

; (define (revappend a b)
;   ;; append a onto b in reverse order
;   (let lp ((a a)
; 	   (b b))
;     (cond ((null? a)
; 	   res)
; 	  ((pair? a)
; 	   (lp (cdr a)
; 	       (cons (car a) b)))
; 	  (else
; 	   ;; (old Q: tell function name?)
; 	   (error "improper list ending in:" a)))))

; (define (stream-union lt . ss)
;   (let union ((ss ss))
;     (cond ((null? ss)
; 	   (error "can't build the union of no input streams"))
; 	  ((null? (cdr ss))
; 	   (car ss))
; 	  (else
; 	   (cars+cdrs ss
; 		      (lambda (cars cdrs)
; 			;; idea to convert cars and cdrs to vectors and lp with mutating those. o well.
; 			...)
; 		      (lambda (seen remain)
; 			;; retry without the list that ended
; 			(union (revappend seen remain))))))))


;; stream-union does not remove doubles

;; (the implementation could be made more efficient (linear factor only))

;; note that "union" may be a misleading name, the lists/streams have
;; to be sorted according to |less?| for it to work. call it merge
;; instead?
(define-strict-and-lazy
  list-union
  stream-union
  (lambda (less? . ss)
    ;; adapted from slib-sort
    (define (merge a b)
      (FV (a b)
	  (cond ((null? a) b)
		((null? b) a)
		(else
		 (let loop ((x (car a))
			    (a (cdr a))
			    (y (car b))
			    (b (cdr b)))
		   ;; x and y are forced, a and b are not.
		   ;; The loop handles the merging of non-empty lists.
		   ;; It has been written this way to save testing and
		   ;; car/cdring.
		   (DELAY
		    (if (less? y x)
			(FV (b)
			    (if (null? b)
				(cons y (cons x a))
				(cons y (loop x
					      a
					      (FORCE (car b))
					      (cdr b)))))
			;; x <= y
			(FV (a)
			    (if (null? a)
				(cons x (cons y b))
				(cons x (loop (FORCE (car a))
					      (cdr a)
					      y
					      b)))))))))))

    (cond ((null? ss)
	   (error "can't build the union of no inputs"))
	  (else
	   (DELAY
	    (let lp ((s (car ss))
		     (ss (cdr ss)))
	      (if (null? ss)
		  s
		  (lp (merge s (car ss))
		      (cdr ss)))))))))

(TEST
 > (F (stream-union < '(1 11 13 14 27 47 61 84 93 98 99)
		    '(31 35 49 65 68 74 74 88 93 94 98)))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (F (stream-take (stream-union < '(1 11 13 14 27 47 61 84 93 98 . 99)
				 '(31 35 49 65 68 74 74 88 93 94 . 98)) 10))
 (1 11 13 14 27 31 35 47 49 61)
 > (list-union < '(1 11 13 14) '(27 47 61 84 93 98 99)
	       '(31 35 49 65 68 74 74 88 93 94 98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (list-union < '(1 11 13 14) '(27 47 61 84 93 98 99)
	       '(31 35 49 65 68 74 74) '(88 93 94) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (list-union < '(1 11 13 14) '(27 47 61 84 93 98 99)
	       '(88 93 94) '(31 35 49 65 68 74 74) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (list-union < '(1 11 13 14) '(27 47 61 84 93 98 99)
	       '(88 93 94) '(31 35 49 65 68 74 74) '())
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 > (list-union < '() '(27 47 61 84 93 98 99) '(88 93 94) '(1 11 13 14)
	       '(31 35 49 65 68 74 74))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 > (list-union < '())
 ()
 > (list-union < '() '(88 93 94))
 (88 93 94)
 ;; (antitests or so:
 > (list-union = '(1 11 13 14 27 47 61 84 93 98 99)
	       '(31 35 49 65 68 74 74 88 93 94 98))
 (1 11 13 14 27 47 61 84 93 98 99 31 35 49 65 68 74 74 88 93 94 98)
 > (list-union > '(1 11 13 14 27 47 61 84 93 98 99)
	       '(31 35 49 65 68 74 74 88 93 94 98))
 (31 35 49 65 68 74 74 88 93 94 98 1 11 13 14 27 47 61 84 93 98 99)
 ;; )
 )

(define-strict-and-lazy
  list-uniq
  stream-uniq
  (lambda (equal? s #!optional (tail '()))
    (DELAY
     (FV (s)
	 (if (null? s)
	     tail
	     (let-pair
	      ((a r) s)
	      (cons
	       a
	       (let rec ((prev a)
			 (s r))
		 (DELAY
		  (let lp ((s s))
		    (FV (s)
			(if (null? s)
			    tail
			    (let-pair ((a r) s)
				      (if (equal? prev a)
					  (lp r)
					  (cons a
						(rec a r))))))))))))))))

(define-strict-and-lazy
  list-uniq-count
  stream-uniq-count
  (lambda (equal? s #!optional (tail '()))
    (DELAY
     (FV (s)
	 (if (null? s)
	     tail
	     (let-pair
	      ((a r) s)
	      (let rec ((element a)
			(s r))
		(DELAY
		 (let lp ((s s)
			  (count 1))
		   (FV (s)
		       (if (null? s)
			   (cons (cons count element)
				 tail)
			   (let-pair ((a r) s)
				     (if (equal? element a)
					 (lp r (inc count))
					 (cons (cons count element)
					       (rec a r)))))))))))))))

;; ^ this algorithm even seems cleaner than the above, XX merge with
;; *-uniq


(TEST
 > (list-uniq-count = '(1 2 3 3 4 5))
 ((1 . 1) (1 . 2) (2 . 3) (1 . 4) (1 . 5))
 > (list-uniq = '(1 1 2 3 4 4.0 4 5 7 1))
 (1 2 3 4 5 7 1)
 > (list-uniq-count = '(1 1 2 3 4 4.0 4 5 7 1 1))
 ((2 . 1) (1 . 2) (1 . 3) (3 . 4) (1 . 5) (1 . 7) (2 . 1))
 > (list-uniq = '(1))
 (1)
 > (list-uniq = '(1 1))
 (1)
 > (list-uniq = '(1 2))
 (1 2)
 > (list-uniq = '())
 ()
 > (list-uniq eq? '() 'c)
 c
 > (list-uniq eq? '(a a b) 'c)
 (a b . c)
 > (define s (stream-uniq eq? '(a) (delay (cons 'a '()))))
 > (promise? s)
 #t
 > (car (force s))
 a
 > (promise? (cdr (force s)))
 #t
 > (force (cdr (force s)))
 (a)
 > (define s (stream-uniq eq? '(a a) (delay (cons 'b '()))))
 > (force (cdr (force s)))
 (b)
 > (promise? (cdr (force s)))
 #t
 )

;; move to future cmp module?
(define (ltfn->cmpfn f)
  (lambda (cmp . args)
    (apply f (cmp->lt? cmp) args)))

(define cmp-list-union (ltfn->cmpfn list-union))
(define cmp-stream-union (ltfn->cmpfn stream-union))

(TEST
 ;; just random adapted COPY from above
 > (cmp-list-union real-cmp '(1 11 13 14) '(27 47 61 84 93 98 99)
		   '(31 35 49 65 68 74 74 88 93 94 98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union real-cmp '(1 11 13 14) '(27 47 61 84 93 98 99)
		   '(31 35 49 65 68 74 74) '(88 93 94) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union real-cmp '(1 11 13 14) '(27 47 61 84 93 98 99)
		   '(88 93 94) '(31 35 49 65 68 74 74) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union real-cmp '(1 11 13 14) '(27 47 61 84 93 98 99)
		   '(88 93 94) '(31 35 49 65 68 74 74) '())
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 > (cmp-list-union real-cmp '() '(27 47 61 84 93 98 99) '(88 93 94)
		   '(1 11 13 14) '(31 35 49 65 68 74 74))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 )

(define (equalfn->cmpfn f)
  (lambda (cmp . args)
    (apply f (cmp->equal? cmp) args)))

(define cmp-list-uniq (equalfn->cmpfn list-uniq))
(define cmp-stream-uniq (equalfn->cmpfn stream-uniq))

(TEST
 > (cmp-list-uniq real-cmp '(1 1 2 3 4 4.0 4 5 7 1))
 (1 2 3 4 5 7 1)
 > (cmp-list-uniq real-cmp '(1))
 (1)
 )

;; XX grr, Haskell *does* call this groupBy (although it also has
;; group, but that uses the default comparison function for the given
;; data type)

(define-strict-and-lazy
  list-group
  stream-group
  ;; each group is built eagerly and contains the items in reverse order
  (lambda (equal? s #!optional (tail '()))
    (DELAY
     (FV (s)
	 (if (null? s)
	     tail
	     (let-pair
	      ((a r) s)
	      (let rec ((prev a)
			(s r))
		(DELAY
		 (let lp ((s s)
			  (group (cons prev '())))
		   (FV (s)
		       (if (null? s)
			   (cons group tail) ;;end?ç
			   (let-pair ((a r) s)
				     (if (equal? prev a)
					 (lp r
					     (cons a group))
					 (cons group
					       (rec a r)))))))))))))))

(TEST
 > (list-group equal? '(1 2 2 3 4 5))
 ((1) (2 2) (3) (4) (5))
 > (list-group equal? '(2 2))
 ((2 2))
 > (list-group equal? '(2))
 ((2))
 > (list-group equal? '())
 ()
 )

(define cmp-list-group (equalfn->cmpfn list-group))
(define cmp-stream-group (equalfn->cmpfn stream-group))


;; define groupn function:

;; http://api.call-cc.org/doc/data-structures#sec:chop
;; https://hackage.haskell.org/package/split-0.2.3.1/docs/Data-List-Split.html#v:chunksOf
;; <jcowan> group-by-size works for me

;; Also see sequential-pairs and sequentialpairs->pairs in dsssl.scm

(define-strict-and-lazy
  chop/map
  stream-chop/map
  (typed-lambda (#(natural? n) s #(procedure? f) #!optional (tail '()))
	   (let buildup ((s s)
			 (l '())
			 (m n))
	     (DELAY
	      (FV (s)
		  (if (null? s)
		      (if (null? l)
			  tail
			  (cons (f l) tail))
		      (let-pair ((a s*) s)
				(let ((l* (cons a l)))
				  (if (<= m 1)
				      (cons (f l*)
					    (buildup s* '() n))
				      (buildup s* l* (dec m)))))))))))

(define (chop n s #!optional (tail '()))
  (chop/map n s reverse tail))

(define (stream-chop n s #!optional (tail '()))
  (stream-chop/map n s reverse tail))

(TEST
 > (chop 10 '(a b c))
 ((a b c))
 > (chop 1 '(a b c d e))
 ((a) (b) (c) (d) (e))
 > (chop 2 '(a b c d e))
 ((a b) (c d) (e))
 > (chop 2 '(a b))
 ((a b))
 > (chop 2 '(a))
 ((a))
 > (chop 2 '())
 ()
 > (chop 3 '(a b c d e f))
 ((a b c) (d e f))
 > (chop 4 '(a b c d e))
 ((a b c d) (e))
 ;; ok?:
 > (%try-error (chop 0 '(a b c d e)))
 #(error "n does not match natural?:" 0)
 > (%try-error (chop -1 '(a b c d e)))
 #(error "n does not match natural?:" -1))




(define (stream-unfold p f g seed #!optional maybe-tail-gen)
  (let recur ((seed seed))
    (if (p seed)
	(if maybe-tail-gen
	    (maybe-tail-gen seed)
	    '())
	(delay (cons (f seed) (recur (g seed)))))))

(TEST 
 > (define r (stream-unfold
	      (lambda-values ((id l)) (null? l))
	      (lambda-values ((id l)) (cons id (car l)))
	      (lambda-values ((id l)) (values (inc id) (cdr l)))
	      (values 10 '(a b c))))
 > (F r)
 ((10 . a) (11 . b) (12 . c))
 > (promise? (cdr (force (cdr (force r)))))
 #t
 > (force (cdr (force (cdr (force r)))))
 ((12 . c))
 )

;; variant of stream-unfold that does f and g at once:
(define (stream-unfold2 p f+g seed #!optional maybe-tail-gen)
  (let recur ((seed seed))
    (if (p seed)
	(if maybe-tail-gen
	    (maybe-tail-gen seed)
	    '())
	(delay
	  (letv ((f* g*) (f+g seed))
		(cons f* (recur g*)))))))

(TEST
 ;; nested "values" :)
 > (define r (stream-unfold2
	      (lambda-values ((id l)) (null? l))
	      (lambda-values ((id l))
			     (values (cons id (car l))
				     (values (inc id) (cdr l))))
	      (values 10 '(a b c))))
 ;; same results as for stream-unfold..
 > (F r)
 ((10 . a) (11 . b) (12 . c))
 > (promise? (cdr (force (cdr (force r)))))
 #t
 > (force (cdr (force (cdr (force r)))))
 ((12 . c))
 )


;; (define (stream-any fn ss)
;;   ( ))

;; srfi-1: (define (zip list1 . more-lists) (apply map list list1 more-lists))
;; hm gives error unless all are of the same length.
;; usually not what we want right? e.g. when zipping in an infinite iota.
;; thus..

(define (stream-zip . ss)
  (let rec ((ss ss))
    (delay
      (if (any (compose null? force) ss)
	  '()
	  (cons (map (lambda (s)
		       (car (force s)))
		     ss)
		(rec (map (lambda (s)
			    (cdr (force s)))
			  ss)))))))

(TEST
 > (stream->list (stream-zip '(a b c) '(1 2)))
 ((a 1) (b 2))
 > (stream->list (stream-zip '(a b) (stream-iota)))
 ((a 0) (b 1))
 )


;; Variant that delivers values tuples
(define-strict-and-lazy
  zip2
  stream-zip2
  (named rec
	 (lambda (l1 l2)
	   (DELAY
	    (FV (l1 l2)
		(if (or (null? l1)
			(null? l2))
		    '()
		    (cons (values (car l1)
				  (car l2))
			  (rec (cdr l1)
			       (cdr l2)))))))))

(TEST
 > (F (stream-map values->vector (stream-zip2 (stream-iota) (list "a" "b"))))
 (#(0 "a") #(1 "b"))
 > (.show (zip2 '(1 2) '(a b)))
 (list (values 1 'a) (values 2 'b)))


;; adapted copies from SRFI-1
(define (stream-find-tail pred list)
  (let lp ((list list))
    (FV (list)
	(and (not (null-list? list))
	     (if (pred (car list)) list
		 (lp (cdr list)))))))

(define (stream-take-while pred lis)
  (let recur ((lis lis))
    (delay
      (FV (lis)
	  (if (null-list? lis) '()
	      (let ((x (car lis)))
		(if (pred x)
		    (cons x (recur (cdr lis)))
		    '())))))))

(define (stream-drop-while pred l)
  (let lp ((l l))
    (FV (l)
	(if (null? l) '()
	    (if (pred (car l))
		(lp (cdr l))
		l)))))

(TEST
 > (F (stream-take (stream-drop-while (cut < <> 10) (stream-iota)) 3))
 (10 11 12)
 )


(define-typed (stream-ref s #(natural0? i))
  (let rec ((s s) (i i))
    (FV (s)
	(if (zero? i)
	    (car s)
	    (rec (cdr s) (dec i))))))

(TEST
 > (stream-ref '(a b) 0)
 a
 > (stream-ref '(a b) 1)
 b
 )

;; adapted from srfi-1: (copyright: see srfi-1.scm)

(define (stream-%cars+cdrs lists)
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((lists lists))
       (if (pair? lists)
	   (receive
	    (list other-lists) (car+cdr lists)
	    (FV (list)
		(if (null-list? list)
		    (abort '() '()) ; LIST is empty -- bail out
		    (receive (a d) (car+cdr list)
			     (receive (cars cdrs) (recur other-lists)
				      (values (cons a cars) (cons d cdrs)))))))
	   (values '() '()))))))

(define (stream-every pred lis1 . lists)
  ;;(check-arg procedure? pred every)
  (if (pair? lists)

      ;; N-ary case
      (receive (heads tails) (stream-%cars+cdrs (cons lis1 lists))
	       (or (not (pair? heads))
		   (let lp ((heads heads) (tails tails))
		     (receive (next-heads next-tails) (stream-%cars+cdrs tails)
			      (if (pair? next-heads)
				  (and (apply pred heads)
				       (lp next-heads next-tails))
				  (apply pred heads))))))

      ;; Fast path
      (FV (lis1)
	  (or (null-list? lis1)
	      (let lp ((head (car lis1))
		       (tail (cdr lis1)))
		(FV (tail)
		    (if (null-list? tail)
			(pred head)	; Last PRED app is tail call.
			(and (pred head) (lp (car tail) (cdr tail))))))))))

(TEST
 > (stream-every (lambda (a b) (a b)) (list even?) '(0 2))
 #t
 > (stream-every (lambda (a b) (a b)) (list even? odd?) '(0 2))
 #f
 > (stream-every (lambda (a b) (a b)) (list even? odd?) '(0 3))
 #t
 > (stream-every (lambda (a b) (a b)) (list even? odd?) (stream-iota))
 #t
 > (stream-every (lambda (a b) (a b)) (list even? even?) (stream-iota))
 #f)



(define (gen-infinite-stream get)
  (let lp ()
    (delay
      (cons (get) (lp)))))

(define (gen-stream get eof? #!optional (tail '()))
  (let lp ()
    (delay
      (let ((v (get)))
	(if (eof? v)
	    tail
	    (cons v (lp)))))))

(TEST
 > (define s (gen-infinite-stream (lambda () 1)))
 > (F (stream-take s 3))
 (1 1 1)
 > (define s1 (gen-stream (let ((c 0))
			    (lambda ()
			      (inc! c)
			      c))
			  (lambda (_) (> _ 5))))
 > (F s1)
 (1 2 3 4 5)
 > (promise? s1)
 #t)


;; Also see list-preferred

(define (stream-min&max s
			#!key
			(cmp generic-cmp)
			all?)
  (let ((con (lambda (v r)
	       (if all? (cons v r) v)))
	(ex (lambda (vS)
	      (if all? (car vS) vS))))

    (let-pair ((v s*) (force s))

	      (let lp ((s s)
		       (min (con v '()))
		       (max (con v '())))
		(FV (s)
		    (if (null? s)
			(values min max)
			(let-pair ((v s*) s)
				  (lp s*
				      (match-cmp (cmp (ex min) v)
						 ((lt) min)
						 ((gt) (con v '()))
						 ((eq) (con v min)))
				      (match-cmp (cmp (ex max) v)
						 ((lt) (con v '()))
						 ((gt) max)
						 ((eq) (con v max)))))))))))

(define stream-min
  (compose* fst stream-min&max))

(define stream-max
  (compose* snd stream-min&max))

(TEST
 > (values->vector (stream-min&max '(3 5 9 -3 7)))
 #(-3 9)
 > (values->vector (stream-min&max '(3 5 9 -3 9 7)))
 #(-3 9)
 > (values->vector (stream-min&max '(3)))
 #(3 3)
 > (def l '((3 a) (5 b) (9 c) (-3 d) (7 e) (9 f) (8 g)))
 > (stream-max l cmp: (on car real-cmp))
 (9 f)
 > (stream-max l cmp: (on car real-cmp) all?: #t)
 ((9 f) (9 c)))


(define (stream-sum s)
  (stream-fold + 0 s))


(define (stream-first v)
  (first (force v)))
(define stream-second (cut stream-ref <> 2))
(define stream-third (cut stream-ref <> 3))
(define stream-fourth (cut stream-ref <> 4))
(define stream-fifth (cut stream-ref <> 5))
(define stream-sixth (cut stream-ref <> 6))
(define stream-seventh (cut stream-ref <> 7))
(define stream-eighth (cut stream-ref <> 8))
(define stream-ninth (cut stream-ref <> 9))
(define stream-tenth (cut stream-ref <> 10))
