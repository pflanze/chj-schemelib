;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo
	 jclass
	 test
	 ;; for tests only
	 cj-functional-2
	 slib-sort)

(export (jclass hashcollection)
	(method list.hashcollection))


;; XX currently without a key accessor; add this, or is hash: argument
;; actually all we need?

(jclass (hashcollection hashtable)

	(def-method (contains? c item)
	  (table-ref hashtable item #f))

	(def-method (add! c item)
	  (table-set! hashtable item #t)
	  c)

	(def-method (remove! c item)
	  (table-set! hashtable item)
	  c)

	(def-method (list c)
	  (let ((l '()))
	    (table-for-each (lambda (k v)
			      (push! l k))
			    hashtable)
	    l)))

(define. (list.hashcollection l . options)
  (let* ((len (length l))
	 (t (apply make-table size: len options)))
    (let lp ((l l))
      (if (null? l)
	  (hashcollection t)
	  (let-pair ((a l*) l)
		    (table-set! t a #t)
		    (lp l*))))))


(TEST
 > (def c (.hashcollection '(1 23 40)))
 > (.contains? c 3)
 #f
 > (.contains? c 1)
 #t
 > (.contains? c 40)
 #t
 > (=> c (.add! 3) (.contains? 3))
 #t
 > (=> c (.remove! 40) (.contains? 40))
 #f
 > (sort (.list c) <)
 (1 3 23))

