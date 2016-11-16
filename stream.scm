;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 lazy
	 define-strict-and-lazy
	 cj-struct
	 list-util
	 cj-cmp
	 srfi-11
	 cj-typed)

(export stream-filter/tail
	stream-for-each
	stream-fold-right
	stream:fold-right
	stream-map/tail
	stream-map1
	stream-map
	stream-improper-map
	stream->list
	stream-drop
	stream-take
	stream-sublist
	stream-length
	(struct stream-difference-at)
	(struct stream-no-difference)
	stream-difference
	show-stream-difference
	stream-equal?
	stream-filter
	stream-fold-left stream-fold
	stream-append-optimized
	stream-append/2
	stream-append
	stream-iota
	;; huh why here? XX vs test-lib ?
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
	stream-zip2
	stream-drop-while
	stream-ref
	stream-xone
	;; stream-%cars+cdrs
	stream-every
	gen-infinite-stream
	gen-stream
	stream-min&max
	stream-map/iota)


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
	     ;; (don't keep a reference to the stream head to avoid
	     ;; memory retention!)
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
	     ;; (don't keep a reference to the stream head to avoid
	     ;; memory retention!)
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


(define (stream-append-optimized s1 s2)
  (if (null? (force s2))
      s1
      (stream-append s1 s2)))

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
 > (cmp-list-union number-cmp '(1 11 13 14) '(27 47 61 84 93 98 99)
		   '(31 35 49 65 68 74 74 88 93 94 98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union number-cmp '(1 11 13 14) '(27 47 61 84 93 98 99)
		   '(31 35 49 65 68 74 74) '(88 93 94) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union number-cmp '(1 11 13 14) '(27 47 61 84 93 98 99)
		   '(88 93 94) '(31 35 49 65 68 74 74) '(98))
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 98 99)
 > (cmp-list-union number-cmp '(1 11 13 14) '(27 47 61 84 93 98 99)
		   '(88 93 94) '(31 35 49 65 68 74 74) '())
 (1 11 13 14 27 31 35 47 49 61 65 68 74 74 84 88 93 93 94 98 99)
 > (cmp-list-union number-cmp '() '(27 47 61 84 93 98 99) '(88 93 94)
		   '(1 11 13 14) '(31 35 49 65 68 74 74))
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
(define (stream-zip2 l1 l2)
  (delay
    (FV (l1 l2)
	(if (or (null? l1)
		(null? l2))
	    '()
	    (cons (values (car l1)
			  (car l2))
		  (stream-zip2 (cdr l1)
			       (cdr l2)))))))

(TEST
 > (F (stream-map values->vector (stream-zip2 (stream-iota) (list "a" "b"))))
 (#(0 "a") #(1 "b")))


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


(define (stream-min&max s)
  (let-pair
   ((a s*) (force s))
   (stream-fold-left (lambda-values (v (lo hi))
			       (values (min v lo)
				       (max v hi)))
		     (values a a)
		     s*)))

(TEST
 > (.vector (stream-min&max '(3 5 9 -3 7)))
 #(-3 9)
 > (.vector (stream-min&max '(3)))
 #(3 3))


;; adapted (list-util-1 map/iota)
(define (stream-map/iota fn lis)
  (let rec ((lis lis)
	    (i 0))
    (delay
      (FV (lis)
	  (if (null? lis) lis
	      (cons (fn (car lis) i)
		    (rec (cdr lis) (inc i))))))))

