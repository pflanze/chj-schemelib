;;; Copyright 2014-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test)

(export mapfn
	#!optional
	interpolate)


(def (cond-ordered-assoc x l < before between after)
     (if (< x (caar l))
	 (before (car l))
	 (let lp ((l (cdr l))
		  (prev-a (car l)))
	   (if (null? l)
	       (after prev-a)
	       (let-pair ((a l*) l)
			 (if (< x (car a))
			     (between prev-a a)
			     (lp l* a)))))))

(defstruct before value)
(defstruct on-or-after value)
(defstruct between fst snd)

(def (ordered-assoc x l #!optional (< <))
     (cond-ordered-assoc x l < before between on-or-after))

(TEST
 > (ordered-assoc 0 '((1 a) (2 b) (3 c)))
 #((before) (1 a))
 > (ordered-assoc 1 '((1 a) (2 b) (3 c)))
 #((between) (1 a) (2 b))
 > (ordered-assoc 1.1 '((1 a) (2 b) (3 c)))
 #((between) (1 a) (2 b))
 > (ordered-assoc 2 '((1 a) (2 b) (3 c)))
 #((between) (2 b) (3 c))
 > (ordered-assoc 2.1 '((1 a) (2 b) (3 c)))
 #((between) (2 b) (3 c))
 > (ordered-assoc 3 '((1 a) (2 b) (3 c)))
 #((on-or-after) (3 c))
 )

(def (interpolate p1 p2 x)
     (let-pair ((x1 y1) p1)
	       (let-pair ((x2 y2) p2)
			 (+ y1
			    (* (- y2 y1)
			       (/ (- x x1) (- x2 x1)))))))

(def (mapfn #((list-of (pair-of real? number?)) alis))
     (let ((l (sort alis (on car <))))
       (typed-lambda (#(real? x))
		     (cond-ordered-assoc
		      x l <
		      (lambda-pair ((x1 y1))
			      (error "out of range, value too small:" x x1))
		      (C interpolate _ _ x)
		      (lambda-pair ((x1 y1))
			      (if (= x x1)
				  y1
				  (error "out of range, value too big:" x x1)))))))


(TEST
 > (def m (mapfn '((1 . 100) (2 . 200) (0 . 10) (3 . 310))))
 > (m 0)
 10
 > (m 1)
 100
 > (m 3)
 310
 > (%try-error (m 3.1))
 #(error "out of range, value too big:" 3.1 3)
 > (%try-error (m -1))
 #(error "out of range, value too small:" -1 0)
 > (m 0.5)
 55.
 > (m 1.5)
 150.
 )
