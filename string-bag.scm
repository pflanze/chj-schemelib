;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test)

;; A "string-bag" is an improper list of strings and string-bags.
;; (vs. Rope?)

(def (string-bag-length b)
     (let lp ((n 0)
	      (b b))
       (xcond ((pair? b)
	       (let-pair ((a b*) b)
			 (lp (lp n a) ;; (+ n (string-bag-length a)) ah, heh
			     b*)))
	      ((string? b)
	       (+ n (string-length b)))
	      ((null? b)
	       n))))

(def (string-bag->string b)
     (let* ((len (string-bag-length b))
	    (out (##make-string len)))
       (assert (= (let copy! ;; returns the i after finishing
		      ((i 0)
		       (b b))
		    (xcond ((pair? b)
			    (let-pair ((a b*) b)
				      (copy! (copy! i a)
					     b*)))
			   ((string? b)
			    (let ((l (string-length b)))
			      (string-copy! out i b 0 l)
			      (+ i l)))
			   ((null? b)
			    i)))
		  len))
       out))

;; ^ XX should abstract this, really!!

(TEST
 > (string-bag->string "")
 ""
 > (string-bag->string '())
 ""
 > (string-bag->string "ab")
 "ab"
 > (string-bag->string '("a" "b"))
 "ab"
 > (string-bag->string '("a" ("b" () . "c")))
 "abc")

