;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; a simple string-only, vector-as-map based trie

;; XX Stupid, could trivially (except for helper stuff) change into a
;; trie accepting lists of anything for which there is a .cmp

;; XX NOTE: this currently has bad complexity for the add operation
;; due to copying whole vectors (may not matter for actual usage but
;; be warned).

(require easy
	 vector-binsearch
	 Maybe)

(export (class string-trie)
	#!optional
	string-trie-map-vector?)

(include "cj-standarddeclares.scm")

;; XX move

(def strings? (list-of string?))

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

;;/move


(defclass (string-trie [string-trie-map-vector? entries]
		       [Maybe? Maybe-value])

  (def (entries-alist.string-trie entries-alist Maybe-value)
       (string-trie (alist.string-trie-map-vector entries-alist)
		    Maybe-value))
    
  (defmethod (show s)
    `(entries-alist.string-trie
      ,(.show (string-trie-map-vector.alist entries))
      ,(.show Maybe-value)))

  (defmethod (Maybe-ref-list s chars)
    (if (null? chars)
	Maybe-value
	(let-pair ((c chars*) chars)
		  (if-let ((s* (string-trie-map-vector.maybe-ref entries c)))
			  (string-trie.Maybe-ref-list s* chars*)
			  (Nothing)))))

  (defmethod (Maybe-ref s str)
    ;; XX could optimize
    (string-trie.Maybe-ref-list s (string.list str)))

  (defmethod- (add/set-list s key-chars val
			    orig-key add?)
    (let rec ((s s)
	      (cs key-chars))
      (if (null? cs)
	  (if (and add? (not (Nothing? (string-trie.Maybe-value s))))
	      (error "key already set" orig-key)
	      (string-trie.Maybe-value-set s (Just val)))
	  (let-pair ((c cs*) cs)
		    (let ((entries (string-trie.entries s)))
		      (string-trie.entries-set
		       s
		       (string-trie-map-vector.set
			entries
			c
			(rec (or (string-trie-map-vector.maybe-ref entries c)
				 empty-string-trie)
			     cs*))))))))

  (defmethod (add-list s key-chars val)
    (string-trie.add/set-list s key-chars val
			      key-chars #t))

  (defmethod (add s key-str val)
    (string-trie.add/set-list s (string.list key-str) val
			      key-str #t))

  (defmethod (set-list s key-chars val)
    (string-trie.add/set-list s key-chars val
			      key-chars #f))

  (defmethod (set s key-str val)
    (string-trie.add/set-list s (string.list key-str) val
			      key-str #f)))


(def string-trie-map-vector? (sectioned-vector-of/2 char? string-trie?))

(def empty-string-trie (string-trie (vector) (Nothing)))

(def. (strings.string-trie ss)
  (fold (flip string-trie.add) empty-string-trie ss))

(def (string-trie-map-vector.alist entries)
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

(def (alist.string-trie-map-vector as) ;; -> string-trie-map-vector?
     (let* ((as (cmp-sort as (on car char-cmp))) ;; offer sorted-alist. version ?
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
 > (alist.string-trie-map-vector '())
 []
 > (string-trie-map-vector.alist #)
 ()
 > (alist.string-trie-map-vector '((#\a . 10) (#\x . 30) (#\c . -1)))
 [#\a #\c #\x 10 -1 30]
 > (string-trie-map-vector.alist #)
 ((#\a . 10) (#\c . -1) (#\x . 30)))


(def (string-trie-map-vector.set entries c val)
     ;; if key already exists, the result has the same length
     (let* ((len (vector-length entries))
	    (middle (arithmetic-shift len -1)))
       (if-let ((i (vector-binsearch/start+end
		    entries c char-cmp 0 middle)))
	       (let ((entries* (vector-copy entries)))
		 (vector-set! entries* i c)
		 (vector-set! entries* (+ middle i) val)
		 entries*)
	       ;; need to make a larger result than we have. XX could
	       ;; optimize, except that would just be a constant
	       ;; optimization whereas there may be a need to optimize
	       ;; the complexity instead.
	       (=> entries
		   string-trie-map-vector.alist
		   ((flip cons) (cons c val))
		   alist.string-trie-map-vector))))

(def (string-trie-map-vector.maybe-ref entries c)
     (let* ((len (vector-length entries))
	    (middle (arithmetic-shift len -1)))
       (if-let ((i (vector-binsearch/start+end
		    entries c char-cmp 0 middle)))
	       (vector-ref entries (+ middle i))
	       #f)))

(TEST
 > (def tv '[#\a #\c #\x 10 -1 30])
 > (string-trie-map-vector.set tv #\x 40)
 [#\a #\c #\x 10 -1 40]
 > (string-trie-map-vector.set tv #\b 40)
 [#\a #\b #\c #\x 10 40 -1 30]
 > tv
 [#\a #\c #\x 10 -1 30])


(TEST
 > (.show empty-string-trie)
 (entries-alist.string-trie (list) (Nothing))
 > (equal? (entries-alist.string-trie (list) (Nothing)) empty-string-trie)
 #t
 > (def t (.add empty-string-trie "Hi" "world"))
 > (.show t)
 (entries-alist.string-trie
  (list (cons #\H
	      (entries-alist.string-trie
	       (list (cons #\i (entries-alist.string-trie (list) (Just "world"))))
	       (Nothing))))
  (Nothing))
 > (def t2 (.add t "Hj" "2"))
 > (.show (.Maybe-ref t2 "Hi"))
 (Just "world")
 > (.show (.Maybe-ref t2 "H"))
 (Nothing)
 > (.show (.Maybe-ref t2 "Hj"))
 (Just "2")
 > (.show (.Maybe-ref t2 "Hk"))
 (Nothing)
 > (.show (.Maybe-ref t2 "Hi "))
 (Nothing)
 
 > (%try-error (.add t "Hi" "2"))
 [error "key already set" "Hi"]
 > (equal? t (.set t "Hi" "2"))
 #f
 > (equal? t (.set t "Hi" "world"))
 #t
 > (equal? t (=> t (.set "Hi" "2") (.set "Hi" "world")))
 #t
 > (.Maybe-ref t "H")
 [(Nothing)]
 > (def t3 (.set t "H" "fill-in"))
 > (.Maybe-ref t3 "H")
 [(Just) "fill-in"]
 > (.Maybe-ref t3 "Hi")
 [(Just) "world"])

