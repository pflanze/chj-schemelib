;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; a trie based on a vectormap

;; Requires .cmp as well as .list for the non-"-list" methods on the
;; key type.

;; XX NOTE: this currently has bad complexity for the add operation
;; due to copying whole vectors (may not matter for actual usage but
;; be warned).


(require easy
	 vectormap
	 cj-cmp
	 Maybe)

(export (class trie-alike)
	(class trie)
	(method alist.trie)
	#!optional
	trie-alike-vectormap?)

(include "cj-standarddeclares.scm")


(defclass (trie-alike [Maybe? Maybe-value]
		      [trie-alike-vectormap? entries])

  (defmethod (entries-alist s)
    (vectormap.alist entries))

  (defmethod (entries-length s)
    (vectormap.length entries))

  (defmethod (empty? s)
    (vectormap.empty? entries))

  (defmethod (Maybe-ref s str)
    ;; XX could optimize
    (.Maybe-ref-list s (.list str))))



(def trie-alike-vectormap? (sectioned-vector-of/2 any? trie-alike?))



(defclass (trie)
  extends: trie-alike

  (def (Maybe-value&entries-alist->trie Maybe-value entries-alist)
       (trie Maybe-value
	     (alist.vectormap entries-alist .cmp)))
    
  (defmethod (show s show)
    `(Maybe-value&entries-alist->trie
      ,(show Maybe-value)
      ,(show (vectormap.alist entries))))

  ;; report the next branching or value holding point (could be the
  ;; end)
  (defmethod (rnext-branch s [fixnum-natural0? current-level] [ilist? tail])
    ;; new-level rkeys has-value? remainder
    -> (values-of fixnum-natural0? list? boolean? trie?)

    (let lp ((l current-level)
	     (items tail)
	     (t s))
      (if (Just? (trie.Maybe-value t))
	  (values l items #t t)
	  (let* ((es (trie.entries t))
		 (len (vectormap.length es)))
	    (if (= len 1)
		(lp (inc l)
		    (cons (vectormap.@the-key es) items)
		    (vectormap.@the-value es))
		(values l items #f t))))))
  
  (defmethod (next-branch s [fixnum-natural0? current-level])
    (letv ((new-level rkeys has-value? remainder)
	   (trie.rnext-branch s current-level '()))
	  (values new-level (reverse rkeys) has-value? remainder)))

  (defmethod (Maybe-ref-list s cs)
    (if (null? cs)
	Maybe-value
	(let-pair ((c cs*) cs)
		  (if-let ((s* (vectormap.maybe-ref entries c .cmp)))
			  (trie.Maybe-ref-list s* cs*)
			  (Nothing)))))

  (defmethod- (add/set-list s key-cs val
			    orig-key add?)
    (let rec ((s s)
	      (cs key-cs))
      (if (null? cs)
	  (if (and add? (not (Nothing? (trie.Maybe-value s))))
	      (error "key already set" orig-key)
	      (trie.Maybe-value-set s (Just val)))
	  (let-pair ((c cs*) cs)
		    (let ((entries (trie.entries s)))
		      (trie.entries-set
		       s
		       (vectormap.set
			entries
			c
			(rec (or (vectormap.maybe-ref entries c .cmp)
				 empty-trie)
			     cs*)
			.cmp)))))))

  (defmethod (add-list s key-cs val)
    (trie.add/set-list s key-cs val
		       key-cs #t))

  (defmethod (add s key-str val)
    (trie.add/set-list s (.list key-str) val
		       key-str #t))

  (defmethod (set-list s key-cs val)
    (trie.add/set-list s key-cs val
		       key-cs #f))

  (defmethod (set s key-str val)
    (trie.add/set-list s (.list key-str) val
		       key-str #f)))


(def empty-trie (trie (Nothing) (vector)))

(def. (alist.trie ss)
  (fold (lambda (k+v t)
	  (let-pair ((k v) k+v)
		    (trie.add t k v))) empty-trie ss))

(TEST
 > (show empty-trie)
 (Maybe-value&entries-alist->trie (Nothing) (list))
 > (equal? (Maybe-value&entries-alist->trie (Nothing) (list)) empty-trie)
 #t
 > (def t (.add empty-trie "Hi" "world"))
 > (show t)
 (Maybe-value&entries-alist->trie
  (Nothing)
  (list (cons #\H
	      (Maybe-value&entries-alist->trie
	       (Nothing)
	       (list (cons #\i (Maybe-value&entries-alist->trie (Just "world")
								(list))))))))
 > (def t2 (.add t "Hj" "2"))
 > (show (.Maybe-ref t2 "Hi"))
 (Just "world")
 > (show (.Maybe-ref t2 "H"))
 (Nothing)
 > (show (.Maybe-ref t2 "Hj"))
 (Just "2")
 > (show (.Maybe-ref t2 "Hk"))
 (Nothing)
 > (show (.Maybe-ref t2 "Hi "))
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

