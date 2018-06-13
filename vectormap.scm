;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A map implementation based on a vector via binary search.

;; Requires .cmp

;; XX NOTE: complexity for the add operation is bad due to copying
;; whole vectors


(require easy
	 vector-binsearch
	 cj-cmp
	 Maybe)

(export (methods char.cmp
		 string.cmp
		 number.cmp
		 symbol.cmp
		 u8vector.cmp) ;; move
	empty-vectormap
	vector-every/start+end
	sectioned-vector-of/2
	;; these are all functions in spite of dot
	vectormap.alist
	alist.vectormap
	vectormap.set
	vectormap.maybe-ref
	vectormap.length
	vectormap.empty?
	vectormap.@the-key
	vectormap.@the-value)

(include "cj-standarddeclares.scm")

;; XX move

(def. char.cmp char-cmp)
(def. string.cmp string-cmp)
(def. number.cmp number-cmp)
(def. symbol.cmp symbol-cmp)
(def. u8vector.cmp u8vector-cmp)

;;/move

(def empty-vectormap '[])

(def (vector-every/start+end t? v istart iend)
     (let ((len (vector-length v)))
       (and (<= 0 istart len)
	    (<= 0 iend len)
	    (<= istart iend)
	    (let lp ((i istart))
	      (if (< i iend)
		  (and (t? (vector-ref v i))
		       (lp (inc i)))
		  #t)))))

(def (sectioned-vector-of/2 pred1 pred2)
     (lambda (v)
       (and (vector? v)
	    (let ((len (vector-length v)))
	      (and (even? len)
		   (let ((halflen (arithmetic-shift len -1)))
		     (and (vector-every/start+end pred1 v 0 halflen)
			  (vector-every/start+end pred2 v halflen len))))))))

(TEST
 > (def f? (sectioned-vector-of/2 number? symbol?))
 > (f? '[])
 #t
 > (f? '[1 2])
 #f
 > (f? '[1 a])
 #t
 > (f? '[a 1])
 #f
 > (f? '[b a])
 #f
 > (f? '[b])
 #f
 > (f? '[1])
 #f
 > (f? '[1 2 3 a])
 #f
 > (f? '[1 2 3 a b])
 #f
 > (f? '[1 2 3 a b c])
 #t
 > (f? '[1 2 3 a b c d])
 #f)


(def (vectormap.alist entries)
     (let* ((len (vector-length entries))
	    (middle (arithmetic-shift len -1)))
       (let lp ((i (dec middle))
		(out '()))
	 (if (negative? i)
	     out
	     (lp (dec i)
		 (cons (cons (vector-ref entries i)
			     (vector-ref entries (+ middle i)))
		       out))))))

(def (alist.vectormap as) ;; -> vectormap?
     (let* ((as (cmp-sort as (on car .cmp))) ;; offer sorted-alist. version ?
	    (middle (length as))
	    (len (arithmetic-shift middle 1))
	    (entries (make-vector len)))
       (let lp ((as as)
		(i 0))
	 (if (null? as)
	     entries
	     (let-pair ((a as*) as)
		       (let-pair ((k v) a)
				 (vector-set! entries i k)
				 (vector-set! entries (+ middle i) v)
				 (lp as* (inc i))))))))

(TEST
 > (alist.vectormap '())
 []
 > (vectormap.alist #)
 ()
 > (alist.vectormap '((#\a . 10) (#\x . 30) (#\c . -1)))
 [#\a #\c #\x 10 -1 30]
 > (vectormap.alist #)
 ((#\a . 10) (#\c . -1) (#\x . 30)))


(def (vectormap.set entries c val)
     ;; if key already exists, the result has the same length
     (let* ((len (vector-length entries))
	    (middle (arithmetic-shift len -1)))
       (if-let ((i (vector-binsearch/start+end
		    entries c .cmp 0 middle)))
	       (let ((entries* (vector-copy entries)))
		 (vector-set! entries* i c)
		 (vector-set! entries* (+ middle i) val)
		 entries*)
	       ;; need to make a larger result than we have. XX could
	       ;; optimize, except that would just be a constant
	       ;; optimization whereas there may be a need to optimize
	       ;; the complexity instead.
	       (=> entries
		   vectormap.alist
		   ((flip cons) (cons c val))
		   alist.vectormap))))

(def (vectormap.maybe-ref entries c)
     (let* ((len (vector-length entries))
	    (middle (arithmetic-shift len -1)))
       (if-let ((i (vector-binsearch/start+end
		    entries c .cmp 0 middle)))
	       (vector-ref entries (+ middle i))
	       #f)))

(TEST
 > (def tv '[#\a #\c #\x 10 -1 30])
 > (vectormap.set tv #\x 40)
 [#\a #\c #\x 10 -1 40]
 > (vectormap.set tv #\b 40)
 [#\a #\b #\c #\x 10 40 -1 30]
 > tv
 [#\a #\c #\x 10 -1 30])


(def (vectormap.length entries)
     (arithmetic-shift (vector-length entries) -1))

(def (vectormap.empty? entries)
     (zero? (vector-length entries)))

;; assuming 1 entry:
(def (vectormap.@the-key entries)
     (vector-ref entries 0))
(def (vectormap.@the-value entries)
     (vector-ref entries 1))

