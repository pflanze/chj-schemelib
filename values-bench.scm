;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (cj-gambit-sys-0 @vector-ref))

(export values-bench)

(include "cj-standarddeclares.scm")


(def (values-bench n)
     (repeat n (letv ((a b) (let ()
			      (declare (not safe) (fixnum))
			      (values (* n n) (* (+ n 1) n))))
		     b)))

(def (values-bench-unsafe n)
     (repeat n (let ()
		 (declare (not safe))
		 (letv ((a b) (let ()
				(declare (not safe) (fixnum))
				(values (* n n) (* (+ n 1) n))))
		       b))))


(def (values-bench-manual n)
     (repeat n
	     (let ()
	       (let* ((GEN:V-1130
		       (let ()
			 (declare (not safe) (fixnum))
			 (values (* n n) (* (+ n 1) n))))
		      (a (@vector-ref GEN:V-1130 0))
		      (b (@vector-ref GEN:V-1130 1)))
		 b))))

(def (values-bench-call n)
     (repeat n
	     (call-with-values
		 (lambda ()
		   (declare (not safe) (fixnum))
		   (values (* n n) (* (+ n 1) n)))
	       (lambda (a b)
		 b))))


;; compiled, unlike in TEST environment
(def (values-bench:t1)
     (letv (() (values))
	   'ok))

(def (values-bench:t2)
     (letv ((a) (values 2 3 4))
	   a))

(def (values-bench:t3)
     (letv ((a) (values (values 2 3 4)))
	   a))

(TEST
 > (values-bench:t1)
 ok
 > (show (values-bench:t2))
 (values 2 3 4)
 > (show (values-bench:t3))
 (values 2 3 4))


