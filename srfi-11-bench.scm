;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 srfi-11)

(export srfi-11-bench)

(include "cj-standarddeclares.scm")


(def (srfi-11-bench n)
     (repeat n (letv ((a b) (let ()
			      (declare (not safe) (fixnum))
			      (values (* n n) (* (+ n 1) n))))
		     b)))

(def (srfi-11-bench-unsafe n)
     (repeat n (let ()
		 (declare (not safe))
		 (letv ((a b) (let ()
				(declare (not safe) (fixnum))
				(values (* n n) (* (+ n 1) n))))
		       b))))


(def (srfi-11-bench-manual n)
     (repeat n
	     (let ()
	       (let* ((GEN:V-1130
		       (let ()
			 (declare (not safe) (fixnum))
			 (values (* n n) (* (+ n 1) n))))
		      (a (let ()
			   (declare (not safe))
			   (##vector-ref GEN:V-1130 0)))
		      (b (let ()
			   (declare (not safe))
			   (##vector-ref GEN:V-1130 1))))
		 b))))

(def (srfi-11-bench-call n)
     (repeat n
	     (call-with-values
		 (lambda ()
		   (declare (not safe) (fixnum))
		   (values (* n n) (* (+ n 1) n)))
	       (lambda (a b)
		 b))))


;; compiled, unlike in TEST environment
(def (srfi-11-bench:t1)
     (letv (() (values))
	   'ok))

(def (srfi-11-bench:t2)
     (letv ((a) (values 2 3 4))
	   a))

(def (srfi-11-bench:t3)
     (letv ((a) (values (values 2 3 4)))
	   a))

(TEST
 > (srfi-11-bench:t1)
 ok
 > (.show (srfi-11-bench:t2))
 (values 2 3 4)
 > (.show (srfi-11-bench:t3))
 (values 2 3 4))


