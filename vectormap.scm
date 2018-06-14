;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A map implementation based on a vector via binary search.

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
	vectormap.maybe-find
	(macro @vectormap.iref-key)
	(macro @vectormap.iref-value)
	vectormap.maybe-ref
	vectormap.length
	vectormap.empty?
	vectormap.@the-key
	vectormap.@the-value
	vectormap.map)

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

(def (alist.vectormap as cmp) ;; -> vectormap?
     (let* ((as (cmp-sort as (on car cmp))) ;; offer sorted-alist. version ?
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
 > (alist.vectormap '() .cmp)
 []
 > (vectormap.alist #)
 ()
 > (alist.vectormap '((#\a . 10) (#\x . 30) (#\c . -1)) .cmp)
 [#\a #\c #\x 10 -1 30]
 > (vectormap.alist #)
 ((#\a . 10) (#\c . -1) (#\x . 30)))


(def (vectormap.set entries c val cmp)
     ;; if key already exists, the result has the same length
     (let* ((len (vector-length entries))
	    (middle (arithmetic-shift len -1)))
       (if-let ((i (vector-binsearch/start+end
		    entries c cmp 0 middle)))
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
		   (alist.vectormap cmp)))))

(def (vectormap.maybe-find entries c cmp)
     (let* ((len (vector-length entries))
	    (middle (arithmetic-shift len -1)))
       (vector-binsearch/start+end
	entries c cmp 0 middle)))

;; to be used with vectormap.maybe-find, so as to retrieve key and
;; value as desired; be careful not to make any mistakes here, unsafe!
;; Just pass the same entries, and the i retrieved from
;; vectormap.maybe-find (and before anyone shrink!s entries !)
(defmacro (@vectormap.iref-key entries i)
  `(let ()
     (declare (not safe))
     (vector-ref ,entries ,i)))

(defmacro (@vectormap.iref-value entries i)
  (with-gensyms
   (ENTRIES I)
   `(let ((,ENTRIES ,entries)
	  (,I ,i))
      (declare (fixnum) (not safe))
      (vector-ref ,ENTRIES (+ ,I (arithmetic-shift (vector-length ,ENTRIES) -1))))))

(def (vectormap.maybe-ref entries c cmp)
     (if-let ((i (vectormap.maybe-find entries c cmp)))
	     (@vectormap.iref-value entries i)
	     #f))

(TEST
 > (def tv '[#\a #\c #\x 10 -1 30])
 > (vectormap.set tv #\x 40 .cmp)
 [#\a #\c #\x 10 -1 40]
 > (vectormap.set tv #\b 40 .cmp)
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


;; fn receives key and val, and must return (values key* val*)
(def (vectormap.map vm fn/key+val)
     (let* ((siz (vector-length vm))
	    (res (make-vector siz))
	    (len (arithmetic-shift siz -1)))
       (for..< (i 0 len)
	       (let ((i* (+ len i)))
		 (letv ((k* v*) (fn/key+val (vector-ref vm i)
					    (vector-ref vm i*)))
		       (vector-set! res i k*)
		       (vector-set! res i* v*))))
       res))

