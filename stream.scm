;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.test)
	 (lib.lazy)
	 (lib.define-strict-and-lazy)
	 (lib.cj-struct)
	 (lib.list-util)
	 (lib.cj-cmp)
	 (lib.srfi-11)
	 (lib.cj-typed)
	 )


(define (stream-filter/tail pred s tail)
  (let rec ((s s))
    (delay
      (let ((s (force s)))
	(cond ((pair? s)
	       (let ((a (force (car s))))
		 (if (pred a)
		     (cons a
			   (rec (cdr s)))
		     (rec (cdr s)))))
	      ((null? s)
	       tail)
	      (else
	       (error "improper stream:" s)))))))


(TEST
 > (require (lib.lazy-debug)))
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
;  > (stream-for-each pp (Set->stream (Set-intersection (list->Set number-cmp (list 1)) (list->Set number-cmp (list 2)))))
;  > (stream-for-each pp (Set->stream (Set-union (list->Set number-cmp (list 1)) (list->Set number-cmp (list 2)))))
;  1
;  2
)

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
 > (F (stream-map inc (list 10 20 30)))
 (11 21 31)
 > (F (stream-map/tail inc (list 10 20 30) '(a b)))
 (11 21 31 a b)
 > (F (stream-map/tail inc (list) '(a b)))
 (a b)
 > (F (stream-map vector (stream-iota) '(a b c)))
 (#(0 a) #(1 b) #(2 c))
 )


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
	     ;; (don't keep a reference to the stream head to avoid memory retention!)
	     (error "stream->list: improper stream, ending in:" p))))))

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
	     ;; (don't keep a reference to the stream head to avoid memory retention!)
	     (error "stream->list: improper stream, ending in:" p))))))


(define-struct stream-difference-at
  constructor-name: stream-difference-at
  n s1 s2)

(define-struct stream-no-difference
  constructor-name: stream-no-difference
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
    (cond ((stream-no-difference? d)
	   d)
	  (else
	   (let-stream-difference-at
	    ((n s1 s2) d)
	    (list n: n
		  s1: (show s1)
		  s2: (show s2)))))))

(define (stream-difference s1 s2 #!optional (equal? equal?))
  (let lp ((n 0)
	   (s1 s1)
	   (s2 s2))
    (FV (s1 s2)
	(define (differs)
	  (stream-difference-at n s1 s2))
	(if (null? s1)
	    (if (null? s2)
		(stream-no-difference n)
		(differs))
	    (if (null? s2)
		(differs)
		(if (equal? (car s1) (car s2))
		    (lp (inc n) (cdr s1) (cdr s2))
		    (differs)))))))

(define (show-stream-difference s1 s2
				#!key
				(equal? equal?)
				(n 2))
  (show-stream-*-difference
   (stream-difference s1 s2 equal?)
   n))


(define (stream-equal? s1 s2 #!optional (equal? equal?))
  (stream-no-difference? (stream-difference s1 s2 equal?)))

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
 )


(define (stream-filter pred s #!optional (tail '()))
  (let lp ((s s))
    (delay
      (let ((p (force s)))
	(cond ((null? p)
	       tail)
	      ((pair? p)
	       (let ((a (car p))
		     (r (cdr p)))
		 (if (pred a)
		     (cons a
			   (lp r))
		     (lp r))))
	      (else
	       (error "stream-filter: improper stream, ending in:" p)))))))

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


(define (stream-append-optimized s1 s2)
  (if (null? (force s2))
      s1
      (stream-append s1 s2)))

(define (stream-append s1 s2)
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

;; |stream-force-in-background!| returns a stream-forcer (just a thread
;; actually but let's pretend it's an abstract data type)

(define (stream-force-in-background! s
				     #!optional
				     (finished-callback (lambda (len)
							  (void))))
  (let ((th (make-thread/global-parameters
	     (lambda ()
	       (finished-callback (stream-length s))))))
    ;; lower priority is done with lower numbers:
    (thread-base-priority-set! th -10.)
    (thread-start! th)
    th))

;; kill the thread by raising an exception in its context (don't use
;; thread-terminate! because that risks broken state)

(define stream-forcer-kill-exception 'stream-forcer-kill-exception)
(define (stream-forcer-kill-exception? v)
  (eq? v stream-forcer-kill-exception))

(define (stream-forcer-kill! f)
  (##thread-call f
		 (lambda ()
		   (raise stream-forcer-kill-exception))))


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
		   ;; The loop handles the merging of non-empty lists.  It has
		   ;; been written this way to save testing and car/cdring.
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
 > (F (stream-union < '(1 11 13 14 27 47 61 84 93 98 99) '(31 35 49 65 68 74 74 88 93 94 98)))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (F (stream-take (stream-union < '(1 11 13 14 27 47 61 84 93 98 . 99) '(31 35 49 65 68 74 74 88 93 94 . 98)) 10))
 (1 11 13 14 27 31 35 47 49 61)
 > (list-union < '(1 11 13 14) '(27 47 61 84 93 98 99) '(31 35 49 65 68 74 74 88 93 94 98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (list-union < '(1 11 13 14) '(27 47 61 84 93 98 99) '(31 35 49 65 68 74 74) '(88 93 94) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (list-union < '(1 11 13 14) '(27 47 61 84 93 98 99) '(88 93 94) '(31 35 49 65 68 74 74) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (list-union < '(1 11 13 14) '(27 47 61 84 93 98 99) '(88 93 94) '(31 35 49 65 68 74 74) '())
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 > (list-union < '() '(27 47 61 84 93 98 99) '(88 93 94) '(1 11 13 14) '(31 35 49 65 68 74 74))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 > (list-union < '())
 ()
 > (list-union < '() '(88 93 94))
 (88 93 94)
 ;; (antitests or so:
 > (list-union = '(1 11 13 14 27 47 61 84 93 98 99) '(31 35 49 65 68 74 74 88 93 94 98))
 (1 11 13 14 27 47 61 84 93 98 99 31 35 49 65 68 74 74 88 93 94 98)
 > (list-union > '(1 11 13 14 27 47 61 84 93 98 99) '(31 35 49 65 68 74 74 88 93 94 98))
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
 > (cmp-list-union number-cmp '(1 11 13 14) '(27 47 61 84 93 98 99) '(31 35 49 65 68 74 74 88 93 94 98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union number-cmp '(1 11 13 14) '(27 47 61 84 93 98 99) '(31 35 49 65 68 74 74) '(88 93 94) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union number-cmp '(1 11 13 14) '(27 47 61 84 93 98 99) '(88 93 94) '(31 35 49 65 68 74 74) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union number-cmp '(1 11 13 14) '(27 47 61 84 93 98 99) '(88 93 94) '(31 35 49 65 68 74 74) '())
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 > (cmp-list-union number-cmp '() '(27 47 61 84 93 98 99) '(88 93 94) '(1 11 13 14) '(31 35 49 65 68 74 74))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 )

(define (equalfn->cmpfn f)
  (lambda (cmp . args)
    (apply f (cmp->equal? cmp) args)))

(define cmp-list-uniq (equalfn->cmpfn list-uniq))
(define cmp-stream-uniq (equalfn->cmpfn stream-uniq))

(TEST
 > (cmp-list-uniq number-cmp '(1 1 2 3 4 4.0 4 5 7 1))
 (1 2 3 4 5 7 1)
 > (cmp-list-uniq number-cmp '(1))
 (1)
 )

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

;; adapted version of |xone|
(define (stream-xone x #!optional (fail (lambda_ #f)))
  (FV (x)
      (if (pair? x)
	  (if (null? (force (cdr x)))
	      (car x)
	      (fail 'found-too-many))
	  (fail 'not-found))))

;; dito
(define (stream-xxone x)
  (stream-xone x (lambda (e)
		   (error "expected one item, but got:" e x))))
