;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Purely functional pseudo-random stream construction. Too painful to
;; use, thus see test-random.scm instead now. (Might be bearable if
;; using monadic infrastructure; until that's ready, forget about
;; this.)

(require define-macro-star
	 fixnum
         cj-typed
	 test
	 cj-struct
	 list-util
	 (cj-env-2 xcase)
	 stream
	 weak-srfi-1
	 (lazy FV)
	 (lazy-debug F)
	 (cj-math integer natural0.bitsize)
         (cj-functional-2 =>)
         (cj-env IF inc! when)
         (fixnum inc))
;;^ XX which are still used?

(export (struct pseudorandomsource)
	(struct range)
	make-simplerange/base ;; ?
	pseudorandomsource->integer-stream
	pseudorandomsource->real-stream
	pseudorandomsource->char-stream
	pseudorandomsource->a-z-stream
	charstream:string		    ;; ?
	charstream+lenstream->string-stream ;; ?
	pseudorandomsource*->a-z-string-stream
	symbol-stream
	random-sort
	test-random:burn-time!	      ;; ?
	test-random:burn-time-numbers ;; ?
	start-mutator		      ;; ?

        #!optional
        list->chunked-lists ;; XX move?
        )


(include "cj-standarddeclares.scm")


;; pseudorandom sources

(define-struct pseudorandomsource
  i j)

(define (_pseudorandomsource->randomsource prs)
  (let-pseudorandomsource
   ((i j) prs)
   (let ((rs (make-random-source)))
     (random-source-pseudo-randomize! rs i j)
     rs)))

(define-struct simplerange
  base ;; inclusive
  top ;; not inclusive
  )

(define (make-simplerange/base base range)
  (make-simplerange base (+ base range)))
;;^ weird name, admittedly.

(define (pseudorandomsource->integer-stream prs range)
  (let ((random-integer (random-source-make-integers
			 (_pseudorandomsource->randomsource prs))))
    (let-simplerange
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
		    (make-simplerange 0 100))
		   10))
 (35 70 54 50 30 94 61 43 8 0)
 )


(define (pseudorandomsource->char-stream prs range)
  (stream-map integer->char
	      (pseudorandomsource->integer-stream prs range)))

(define (pseudorandomsource->a-z-stream prs)
  (pseudorandomsource->char-stream prs
				   (make-simplerange/base (char->integer #\a)
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
 > (F (stream-take (pseudorandomsource->integer-stream (make-pseudorandomsource 10 13) (make-simplerange 3 6)) 10))
 (5 3 4 5 3 4 3 4 3 3)
 ;; this yields:
 > (F (stream-take (pseudorandomsource*->a-z-string-stream (make-pseudorandomsource 10 11) (make-pseudorandomsource 10 13) (make-simplerange 3 6)) 10))
 ("jsipz" "kqw" "qhro" "cfupo" "kzv" "irqb" "ica" "ctpm" "ivy" "ntj")
 > (F (stream-take (pseudorandomsource*->a-z-string-stream (make-pseudorandomsource 10 11) (make-pseudorandomsource 11 13) (make-simplerange 3 6)) 10))
 ("ipz" "qwjs" "hrok" "upoq" "vcf" "rqbkz" "icai" "tpm" "jivyc" "bbnt")
 > (F (stream-take (pseudorandomsource*->a-z-string-stream (make-pseudorandomsource 12 11) (make-pseudorandomsource 10 13) (make-simplerange 3 6)) 10))
 ("niktx" "igu" "opfo" "lqajo" "wfa" "tpzz" "ehl" "apyc" "uea" "ttq")
 )

(define (symbol-stream namelen-range chars-seed lens-seed)
  (stream-map string->symbol
	      (pseudorandomsource*->a-z-string-stream
	       (make-pseudorandomsource chars-seed 23418)
	       (make-pseudorandomsource lens-seed 9347981)
	       namelen-range)))

(TEST
 > (F (stream-take (symbol-stream (make-simplerange 3 6) 1 2) 10))
 (tlib qfby nykc ahpcb adm aug nwnm dar klkmv zjc)
 > (F (stream-take (symbol-stream (make-simplerange 1 2) 1 2) 10))
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
	(let*-pair
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

(define (test-random:burn-time! n)
  (if (negative? n)
      (thread-sleep! (/ (- n) 1000))
      (let lp ((i n))
	(when (positive? i)
              (lp (dec i))))))

(define (^5/1000 x) (* x x x x x 1/1000))

(define (test-random:burn-time-numbers seed)
  (stream-map ^5/1000
	      (pseudorandomsource->integer-stream
	       (make-pseudorandomsource seed 13342)
	       (make-simplerange -5 20))))

(define (start-mutator mutate!/2 val n seed)
  (let ((xs (test-random:burn-time-numbers seed)))
    (thread-start!
     (make-thread
      (thunk
       (let lp ((i n)
		(xs xs)
		(val val))
	 (if (positive? i)
	     (let*-pair
	      (((x1 xs*) (force xs))
	       ((x2 xs*) (force xs*)))
	      (test-random:burn-time! x1)
	      (lp (dec i)
		  xs*
		  (mutate!/2 (thunk
			      (test-random:burn-time! x2))
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

