;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Functions converting vectors to vectors.

(require easy
	 test)

(export (method vector.filter/iota
		vector.for-each/iota))


(def inc (inline inc))

(def. (vector.filter/iota v fn)
  (let* ((len (vector-length v))
	 (v* (make-vector len)))
    (let lp ((i 0)
	     (j 0))
      (if (fx< i len)
	  (let ((val (vector-ref v i)))
	    (if (fn val i)
		(begin
		  (vector-set! v* j val)
		  (lp (inc i) (inc j)))
		(lp (inc i) j)))
	  (begin
	    (vector-shrink! v* j)
	    v*)))))

(TEST
 > (.filter/iota (vector 2 -4 5 8) (lambda (v i) (even? v)))
 #(2 -4 8)
 > (.filter/iota (vector 2 -4 5 8) (lambda (v i) (even? i)))
 #(2 5))


;; XX move to/merge with vector-util.scm: (vector-for-each proc vec) .. ?

(def. (vector.for-each/iota v proc)
  (let ((len (vector-length v)))
    (for..< (i 0 len)
	    (proc (vector-ref v i)
		  i))))


(TEST
 > (def l '())
 > (def v (vector 10 11 12))
 > (.for-each/iota v (lambda (x i)
		       (push! l (cons x i))))
 > l
 ((12 . 2) (11 . 1) (10 . 0)))


