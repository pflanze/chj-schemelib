;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


(require easy
	 wbtable
	 (test-random make-list!))

(include "cj-standarddeclares.scm")

;; XX move ... also rename number-cmp to real-cmp
(def (fixnum-cmp a b)
     (if (fx< a b)
	 'lt
	 (if (eq? a b)
	     'eq
	     'gt)))

(def (wbtable-bench:run n len)
     (let* ((vals (make-list! len (C random-natural0 100000)))
	    (t1 (time
		 (repeat n
			 (.fold vals
				(lambda (v t)
				  (wbtable.set t v v))
				(empty-wbtable-of fixnum? fixnum-cmp any?))))))
       (time (repeat n (.for-each vals
				  (lambda (v)
				    (wbtable.refx t1 v)))))
       (.length t1)))

