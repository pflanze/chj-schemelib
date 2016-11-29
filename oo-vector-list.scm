;;; Copyright 2014-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Functions converting vectors to lists.

(require easy
	 (cj-functional list-of) ;; part of easy?
	 test)


(export (method vector.map-list)
	(method vectors.map-list))


(def dec (inline dec)) ;; didn't I have a macro for that? lost?


(def. (vector.map-list vec fn #!optional (tail '()))
  (let lp ((i (dec (vector-length vec)))
	   (l tail))
    (if (>= i 0)
	(lp (dec i)
	    (cons (fn (vector-ref vec i)) l))
	l)))


(def vectors? (list-of vector?))

(def. (vectors.map-list vecs fn #!optional (tail '()))
  (let ((len (vector-length (car vecs))))
    (for-each (lambda (v)
		(assert (= (vector-length v) len)))
	      (cdr vecs))
    (let lp ((i (dec len))
	     (l tail))
      (if (>= i 0)
	  (lp (dec i)
	      (cons (apply fn (map (C vector-ref _ i) vecs))
		    l))
	  l))))


(TEST
 > (.map-list (vector 2 3 4) inc)
 (3 4 5)
 > (.map-list (vector 2 3 4) inc 'foo)
 (3 4 5 . foo)
 > (.map-list (list (vector 2 3 4) (vector 1 -1 9)) + 'foo)
 (3 2 13 . foo))

