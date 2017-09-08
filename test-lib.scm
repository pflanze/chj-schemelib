;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 cj-struct
	 (cj-env-2 xcase)
	 stream
	 weak-srfi-1
	 (lazy FV)
	 (lazy-debug F)
	 (cj-math integer natural0.bitsize))

;; A library of helper functions for writing tests
;; also see test-lib-1

(export (struct pseudorandomsource)
	(struct range)
	make-range/base ;; ?
	pseudorandomsource->integer-stream
	pseudorandomsource->real-stream
	pseudorandomsource->char-stream
	pseudorandomsource->a-z-stream
	charstream:string ;; ?
	charstream+lenstream->string-stream ;; ?
	pseudorandomsource*->a-z-string-stream
	symbol-stream
	random-sort
	test-lib:burn-time! ;; ?
	test-lib:burn-time-numbers ;; ?
	start-mutator ;; ?
	random-integer..<
	random-integer..
	random-natural random-natural0
	random-fraction
	produce-stream ;; ?
	random-natural0-above
	random-natural0-above-stream
	test-natural0-exponentials
	test-natural0s

	random-natural0*-big
	random-natural0*-exponential
	random-natural0*
	random-sign
	random-integer*
	random-real-1-1
	random-float
	random-number

	random-boolean
	
	#!optional
	do-iter ;; ?
	list->chunked-lists ;; XX move?
	)

;; pseudorandom sources

(define-struct pseudorandomsource
  i j)

(define (_pseudorandomsource->randomsource prs)
  (let-pseudorandomsource
   ((i j) prs)
   (let ((rs (make-random-source)))
     (random-source-pseudo-randomize! rs i j)
     rs)))

(define-struct range
  base ;; inclusive
  top ;; not inclusive
  )

(define (make-range/base base range)
  (make-range base (+ base range)))
;;^ weird name, admittedly.

(define (pseudorandomsource->integer-stream prs range)
  (let ((random-integer (random-source-make-integers
			 (_pseudorandomsource->randomsource prs))))
    (let-range
     ((base top) range)
     (let ((n (- top base)))
       (let rec ()
	 (delay
	   (cons (+ base (random-integer n))
		 (rec))))))))

(define (pseudorandomsource->real-stream prs)
  (let ((get! (random-source-make-reals
	       (_pseudorandomsource->randomsource prs))))
    (let rec ()
      (delay
	(cons (get!)
	      (rec))))))

(TEST
 > (F (stream-take (pseudorandomsource->integer-stream
		    (make-pseudorandomsource 10 2)
		    (make-range 0 100))
		   10))
 (35 70 54 50 30 94 61 43 8 0)
 )


(define (pseudorandomsource->char-stream prs range)
  (stream-map integer->char
	      (pseudorandomsource->integer-stream prs range)))

(define (pseudorandomsource->a-z-stream prs)
  (pseudorandomsource->char-stream prs
				   (make-range/base (char->integer #\a)
						    26)))


(TEST
 > (let* ((s (pseudorandomsource->a-z-stream (make-pseudorandomsource 11 12))))
     (let*-values (((l1 r1) (stream-rtake&rest s 10))
		   ((l2 r2) (stream-rtake&rest r1 12))
		   ((l r) (stream-rtake&rest s 22)))
		  (list (equal? l1 (reverse (F (stream-take s 10))))
			(equal? l2 (reverse (F (stream-take (stream-drop s 10) 12))))
			(equal? l (append l2 l1))
			(equal? l (append l1 l2)))))
 (#t #t #t #f))

(define (charstream:string charstream len)
  (let-values (((l r) (stream-rtake&rest charstream len)))
	      (values r (list->string l))))

(define (charstream+lenstream->string-stream chars lens)
  (delay
    (FV (lens)
	(letv ((chars* s) (charstream:string chars (car lens)))
	      (cons s
		    (charstream+lenstream->string-stream
		     chars*
		     (cdr lens)))))))

(define (pseudorandomsource*->a-z-string-stream chars-prs lens-prs stringlen-range)
  (charstream+lenstream->string-stream
   (pseudorandomsource->a-z-stream chars-prs)
   (pseudorandomsource->integer-stream lens-prs stringlen-range)))

(TEST
 ;; given:
 > (list->string (F (stream-take (pseudorandomsource->a-z-stream (make-pseudorandomsource 10 11)) 40)))
 "zpisjwqkorhqopufcvzkbqriacimptcyvijtnbbt"
 > (F (stream-take (pseudorandomsource->integer-stream (make-pseudorandomsource 10 13) (make-range 3 6)) 10))
 (5 3 4 5 3 4 3 4 3 3)
 ;; this yields:
 > (F (stream-take (pseudorandomsource*->a-z-string-stream (make-pseudorandomsource 10 11) (make-pseudorandomsource 10 13) (make-range 3 6)) 10))
 ("jsipz" "kqw" "qhro" "cfupo" "kzv" "irqb" "ica" "ctpm" "ivy" "ntj")
 > (F (stream-take (pseudorandomsource*->a-z-string-stream (make-pseudorandomsource 10 11) (make-pseudorandomsource 11 13) (make-range 3 6)) 10))
 ("ipz" "qwjs" "hrok" "upoq" "vcf" "rqbkz" "icai" "tpm" "jivyc" "bbnt")
 > (F (stream-take (pseudorandomsource*->a-z-string-stream (make-pseudorandomsource 12 11) (make-pseudorandomsource 10 13) (make-range 3 6)) 10))
 ("niktx" "igu" "opfo" "lqajo" "wfa" "tpzz" "ehl" "apyc" "uea" "ttq")
 )

(define (symbol-stream namelen-range chars-seed lens-seed)
  (stream-map string->symbol
	      (pseudorandomsource*->a-z-string-stream
	       (make-pseudorandomsource chars-seed 23418)
	       (make-pseudorandomsource lens-seed 9347981)
	       namelen-range)))

(TEST
 > (F (stream-take (symbol-stream (make-range 3 6) 1 2) 10))
 (tlib qfby nykc ahpcb adm aug nwnm dar klkmv zjc)
 > (F (stream-take (symbol-stream (make-range 1 2) 1 2) 10))
 (b i l t y b f q c k) ;; b contained twice..
 )

(define (random-sort reals lis)
  (let lp ((reals reals)
	   (lis lis)
	   (pairlis '()))
    (if (null? lis)
	(values reals
		(map cdr
		     (sort pairlis (on car <))))
	(let-pair*
	 (((v lis*) lis)
	  ((r reals*) (force reals)))
	 (lp reals*
	     lis*
	     (cons (cons r v)
		   pairlis))))))

(TEST
 > (let ((reals (pseudorandomsource->real-stream
		 (make-pseudorandomsource 2 3))))
     (letv ((reals* l)
	    (random-sort reals
			 '(a b c d e)))
	   (vector (equal? (F (stream-take reals* 3))
			   (F (stream-take
			       (stream-drop reals 5)
			       3)))
		   l)))
 #(#t
   (d a c e b))
 )

;; utilities for writing concurrency tests:

(define (test-lib:burn-time! n)
  (if (negative? n)
      (thread-sleep! (/ (- n) 1000))
      (let lp ((i n))
	(if (positive? i)
	    (lp (dec i))))))

(define (^5/1000 x) (* x x x x x 1/1000))

(define (test-lib:burn-time-numbers seed)
  (stream-map ^5/1000
	      (pseudorandomsource->integer-stream
	       (make-pseudorandomsource seed 13342)
	       (make-range -5 20))))

(define (start-mutator mutate!/2 val n seed)
  (let ((xs (test-lib:burn-time-numbers seed)))
    (thread-start!
     (make-thread
      (thunk
       (let lp ((i n)
		(xs xs)
		(val val))
	 (if (positive? i)
	     (let-pair*
	      (((x1 xs*) (force xs))
	       ((x2 xs*) (force xs*)))
	      (test-lib:burn-time! x1)
	      (lp (dec i)
		  xs*
		  (mutate!/2 (thunk
			      (test-lib:burn-time! x2))
			     val)))
	     'end)))))))


;; Build suitable access patterns for n threads doing similar accesses
;; to provoke conflicts: separate a list into chunks, fill each chunk
;; randomly resortede into n new lists.

(define (list->chunked-lists lis n chunksize seed)
  ;; lis: can be a stream of limited length, too
  ;; chunksize: num entries per block which will be randomly reordered
  (let lp ((lis lis)
	   (outliss (make-list n '()))
	   (reals (pseudorandomsource->real-stream
		   (make-pseudorandomsource seed 3334))))
    (if (null? lis)
	outliss
	(letv ((l lis*) (weak-split-at lis chunksize))
	      (let cross ((n n)
			  (outliss outliss)
			  (outliss* '())
			  (reals reals))
		(if (and (positive? n) (pair? outliss))
		    (letv ((reals* l*) (random-sort reals l))
			  (cross (dec n)
				 (cdr outliss)
				 (cons (append l* (car outliss))
				       outliss*)
				 reals*))
		    (lp lis*
			outliss*
			reals)))))))

(TEST
 > (list->chunked-lists '(1a 1b 1c 1d 1e 2f 2g 2h 2i 2j 3k) 3 5 111)
 ((3k 2i 2g 2h 2f 2j 1b 1d 1e 1a 1c)
  (3k 2f 2g 2i 2j 2h 1e 1c 1b 1a 1d)
  (3k 2f 2h 2i 2g 2j 1e 1d 1b 1c 1a))
 )


;; ------------------------------------------------------

;; For side-effecting random testing (with real random inputs, not
;; predetermined):


(define (random-integer..< a b)
  (+ (random-integer (- b a)) a))

(define (random-integer.. a b)
  (random-integer..< a (inc b)))

(define (random-natural below)
  (random-integer..< 1 below))

(define random-natural0 random-integer)

(define (random-fraction lim)
  ;; what numbers to choose? chain number gen to get a sloped
  ;; distribution? correct? XX
  ;;(let ((lim (random-natural0 10000))))
  (/ (random-integer.. (- lim) lim)
     (random-natural lim)))

;; hmm particularly interesting test series:
;; (map random-fraction (iota 30 2))

(define (do-iter n proc)
  (let lp ((i 0))
    (if (< i n)
	(begin (proc i)
	       (lp (inc i))))))

;; (defmacro (%test-iter v+n e)
;;   (mcase v+n
;; 	 (`(`v `n)
;; 	  (assert* symbol? v)
;; 	  (assert* natural0? n)
;; 	  `(do-iter ,n (lambda (,v)
;; 			 (,e))))))


;; name?
(define (produce-stream thunk)
  (delay (cons (thunk)
	       (produce-stream thunk))))

;; how skewed should it be?
(define (random-natural0-above min)
  (let ((x (expt min (/ (random-real)))))
    (if (< x 1e100)
	(integer x)
	(random-natural0-above min))))

(define (random-natural0-above-stream min)
  (produce-stream (lambda ()
		    (random-natural0-above min))))

(define (test-natural0-exponentials
	 #!key
	 (min-bits 0)
	 (len-bits 30))
  (stream-fold-right (lambda (i rest)
		       (let ((n (arithmetic-shift 1 i)))
			 (cons* (dec n)
				n
				(inc n)
				rest)))
		     '()
		     (stream-iota len-bits min-bits)))

;; (define (test-natural0-specials)
;;   )


(define (test-natural0s #!key
			(len-serial 105)
			(len-random 100))
  (stream-append
   ;; all small numbers
   (stream-iota len-serial)
   ;; some bigger special ones
   (test-natural0-exponentials min-bits: (natural0.bitsize len-serial))
   ;; random ones
   (stream-take (random-natural0-above-stream len-serial)
		len-random)))


(define (random-natural0*-big)
  ;; grr was aliasing random-integer to random-natural0 a bad idea?
  ;; Well was already badly named.
  (random-natural0 (arithmetic-shift 1 (random-natural0 100))))

(define (random-natural0*-exponential)
  ;; hmm how exactly anyway, the above is already exponential, but,
  ;; usually small numbers is meant here.
  ;; XXX unfinished
  (random-natural0 (arithmetic-shift 1 (random-natural0 20))))

(define (random-natural0*)
  ;; grr was aliasing random-integer to random-natural0 a bad idea?
  ;; Well was already badly named.
  (xcase (random-integer 3)
	 ((0)
	  (random-natural0*-big))
	 ((1 2)
	  (random-natural0*-exponential))))

(define (random-sign)
  (xcase (random-integer 2)
	 ((0) -1)
	 ((1) 1)))

;; same as random-natural0* except extending into the negative range,
;; too. Might have zeroes twice as frequent, though. (All a hack.)
(define (random-integer*)
  (* (random-sign) (random-natural0*)))


(define (random-real-1-1)
  (- (* (random-real) 2.0) 1.0))

(define (random-float)
  ;; XX what should the limits be? This includes +/- inf at least.
  (* (random-real-1-1) (expt 10 (- (random-integer 700) 350))))

(define (random-number)
  (xcase (random-integer 7)
	 ((0)
	  (random-real-1-1))
	 ((1 2 3)
	  (random-float))
	 ((4 5)
	  (random-natural0*))
	 ((6)
	  (random-integer*))))


(define (random-boolean)
  (xcase (random-integer 2)
	 ((0) #f)
	 ((1) #t)))
