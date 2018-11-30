;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 symboltable)

(include "cj-standarddeclares.scm")

(def (tag)
     (=> 10
	 random-hexstring
	 .symbol
	 list))

(def tag?
     (all-of pair?
	      (=>* car symbol?)))

;; (def maket
;;      (=>* iota
;; 	  (.map (lambda (i)
;; 		  (cons (tag) i)))
;; 	  list->table))

;; much HP48 feeling?:


(def makealis
     (=>* iota
	  (.map (lambda (i)
		  (cons (tag) i)))))

(def maket
     (=>* makealis
	  list->table))


(def (rt [table? t] [tag? k] [exact-natural? n])
     (time (repeat n (table-ref t k))))


(def makes
     (=>* makealis
	  (.map (lambda (kv)
		  (cons (caar kv) kv)))
	  list->symboltable))

(def (rs [symboltable? t] [tag? k] [exact-natural? n])
     (time (repeat n (symboltable-ref t (car k) #f))))


(TEST
 > (def t (maket 100))
 > (exact-natural0? (rt t (=> (.list t) .first car) 4000000))
 #t
 > (def s (makes 100))
 > ((pair-of tag? exact-natural0?)
    (rs s (=> (.list s) .first) 4000000))
 #t)


