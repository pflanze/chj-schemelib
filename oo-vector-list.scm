;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Functions converting vectors to lists.

(require easy
	 (cj-functional list-of) ;; part of easy?
	 test)


(export (method vector.map-list
		vector.map/iota-list
		vectors.map-list
		vectors.map/iota-list))


(def dec (inline dec)) ;; didn't I have a macro for that? lost?


(def. (vector.map-list vec fn #!optional (tail '()) with-i?)
  (let lp ((i (dec (vector-length vec)))
	   (l tail))
    (if (>= i 0)
	(lp (dec i)
	    (cons (let ((v (vector-ref vec i)))
		    (if with-i?
			(fn i v)
			(fn v)))
		  l))
	l)))

(def. (vector.map/iota-list vec fn #!optional (tail '()))
  (vector.map-list vec fn tail #t))


(def vectors? (list-of vector?))

(def. (vectors.map-list vecs fn #!optional (tail '()) with-i?)
  (let ((len (vector-length (car vecs))))
    (for-each (lambda (v)
		(assert (= (vector-length v) len)))
	      (cdr vecs))
    (let lp ((i (dec len))
	     (l tail))
      (if (>= i 0)
	  (lp (dec i)
	      (cons (let ((vs (map (C vector-ref _ i) vecs)))
		      (if with-i?
			  (apply fn i vs)
			  (apply fn vs)))
		    l))
	  l))))


(def. (vectors.map/iota-list vecs fn #!optional (tail '()))
  (vectors.map-list vecs fn tail #t))


(TEST
 > (.map-list (vector 2 3 4) inc)
 (3 4 5)
 > (.map-list (vector 2 3 4) inc 'foo)
 (3 4 5 . foo)
 > (.map-list (list (vector 2 3 4) (vector 1 -1 9)) + 'foo)
 (3 2 13 . foo)
 > (.map/iota-list (vector 2 3 4) + 'foo)
 (2 4 6 . foo)
 > (.map/iota-list (list (vector 2 3 4) (vector 1 -1 9)) + 'foo)
 (3 3 15 . foo))