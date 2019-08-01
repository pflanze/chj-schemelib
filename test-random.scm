;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A library of helper functions for writing tests
;; also see test-lib-1

;; TODO: offer a way to re-init the pseudo-random generator (and print
;; the random seed) for an all-side-effecting operation.

;; These are meant for testing purposes. Do not use in security
;; contexts (see realrandom.scm instead)!  E.g. these are not "fork
;; safe"!

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

(export make-list!
        make-infinite-stream! make-finite-stream! make-stream!

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
        random-length
	random-sign
	random-integer*
	random-real-1-1
	random-float
	random-number
	random-boolean
        ;; random-u8vector see compat.scm
        random-hexstring
	random-filename
	random-char-integer
	random-char
	random-string

        randomly-sized ;; (randomly-sized gen/length)
        randomly-sized/ ;; (randomly-sized/ gen/length)
	
	#!optional
	do-iter		    ;; ?
        expensive-valid-char?
	)


(include "cj-standarddeclares.scm")



(define (make-list! len generate/0)
  (let lp ((i 0)
	   (l '()))
    (if (< i len)
	(lp (inc i)
	    (cons (generate/0) l))
	l)))

(define (make-infinite-stream! proc)
  (let lp ()
    (delay (cons (proc) (lp)))))

(define-typed (make-finite-stream! [fixnum-natural0? n] [procedure? proc])
  (let lp ((i n))
    (delay
      (if (> i 0)
          (cons (proc) (lp (dec i)))
          '()))))

(define (make-stream! a #!optional b)
  (if b
      (make-finite-stream! a b)
      (if (procedure? a)
          (make-infinite-stream! a)
          (error "invalid call"))))

(TEST
 > (define (make-serial-iterator n)
     (lambda ()
       (inc! n)))
 > (=> (make-stream! (make-serial-iterator 0)) (stream-take 3) F)
 (1 2 3)
 > (=> (make-stream! 3 (make-serial-iterator 0)) F)
 (1 2 3))


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
    (when (< i n)
          (proc i)
          (lp (inc i)))))

;; (defmacro (%test-iter v+n e)
;;   (mcase v+n
;; 	 (`(`v `n)
;; 	  (assert* symbol? v)
;; 	  (assert* natural0? n)
;; 	  `(do-iter ,n (lambda (,v)
;; 			 (,e))))))


;; name?
;; XX hm, make-list takes len first.
(define (produce-stream thunk #!optional len)
  (if len
      (let lp ((n len))
        (delay (if (> n 0)
                   (cons (thunk)
                         (lp (dec n)))
                   '())))
      (let lp ()
        (delay (cons (thunk)
                     (lp))))))

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


;; random-length: a natural0 that is small enough to be used as length
;; for strings etc., i.e. real allocations (i.e. not for 32 bit
;; boundary calculations etc.; write a random-size or random-s32
;; etc. if want to do such)

(define (random-length)
  (let ((i (random-integer 10)))
    (if (< i 8)
        i
        (let ((i (random-integer 100)))
          (if (< i 95)
              i
              (let ((i (random-integer 1000)))
                (if (< i 995)
                    i
                    (let ((i (random-integer 10000)))
                      i))))))))


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



(define (random-hexstring len)
  (let* ((s (number->string (random-integer (expt 16 len)) 16))
	 (l (string-length s)))
    (if (= l len)
	s
	(string-append (make-string (- len l) #\0) s))))

(define (random-filename)
  ;; 128 bits
  (random-hexstring 32))


;; find out which integers represent valid chars:

(define (expensive-valid-char? i)
  (with-exception-catcher
   (lambda (e)
     (if (range-exception? e)
	 #f
	 (raise e)))
   (lambda ()
     (integer->char i))))

(IF #f
    (define (char-boundaries)
      (integer->char 0) ;; assert that's valid
      (delay
	(let* ((max-i 9999999999)
	       (stop 'stop))
	  (let lp-valid ((start-valid 0))
	    ;;(warn "start-valid" start-valid)
	    (let lp ((i (inc start-valid)))
	      (if (< i max-i)
		  (if (expensive-valid-char? i)
		      (lp (inc i))
		      (let ()
			;;(warn "end-valid" i)
			(cons (cons start-valid i)
			      (delay
				(let lp ((i (inc i)))
				  (if (< i max-i)
				      (if (expensive-valid-char? i)
					  (lp-valid i)
					  (lp (inc i))
					  )
				      stop))))))
		  (cons (cons start-valid #f)
			stop))))))))

;; > (def bs (char-boundaries))
;; > (stream-ref bs 1)
;; > (S bs)
;; ((0 . 55296)
;;  (57344 . 1114112)
;;  .
;;  [(debuggable-promise) #<procedure #545> #f #<continuation #546>])

;; ^ exclusive the end points.

;; 0 .. #\ud7ff
;; #\ue000 .. #\U0010ffff 

;; https://en.wikipedia.org/wiki/Unicode

;; Unicode defines a codespace of 1,114,112 code points in
;; the range 0hex to 10FFFFhex

;; Code points in the range U+D800–U+DBFF (1,024 code points) are
;; known as high-surrogate code points, and code points in the range
;; U+DC00–U+DFFF (1,024 code points) are known as low-surrogate code
;; points. A high-surrogate code point followed by a low-surrogate
;; code point form a surrogate pair in UTF-16 to represent code points
;; greater than U+FFFF. These code points otherwise cannot be used
;; (this rule is ignored often in practice especially when not using
;; UTF-16).


(define (random-char-integer)
  (declare (fixnum))
  (let ((i (random-integer 300)))
    (if (zero? i)
	0
	(if (< i 40)
	    (+ 57344 (random-integer 1056768))
	    (random-integer 55296)))))

(define (random-char)
  (integer->char (random-char-integer)))

(define (random-string len)
  (declare (fixnum))
  (if (and (fixnum? len)
	   (>= len 0))
      (let ((str (##make-string len)))
	(let lp ((i 0))
	  (if (< i len)
	      (begin
		(string-set! str i (random-char))
		(lp (inc i)))
	      str)))
      (error "not a natural0: " len)))


(define (randomly-sized gen/length)
  (gen/length (random-length)))

(define (randomly-sized/ gen/length)
  (lambda ()
    (randomly-sized gen/length)))

;; well. One is the action, the other returns one. ?


