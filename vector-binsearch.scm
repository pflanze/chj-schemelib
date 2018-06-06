;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 cj-cmp
	 range
	 test
	 test-random)

(export vector-binsearch
	vector-binsearch/start+end
	#!optional
	@vector-binsearch/start+end
	fixnum-natural0?
	range-of-integer?
	sorted-integers.maybe-gap)

(include "cj-standarddeclares.scm")

;;XX move
(def (fixnum-natural0? x)
     (and (fixnum? x)
	  (<= 0 x)))
;; (Be careful, the above is not enough for making sure it's in vector
;; boundaries, right? Well, vectors are not usually their max size
;; anyway, though.)


(def (@vector-binsearch/start+end v val cmp istart iend)
     (let lp ((istart istart)
	      (iend iend))
       (let ((d (fx- iend istart)))
	 (and (fxpositive? d)
	      (let ((i (fx+ istart (arithmetic-shift d -1))))
		(match-cmp (cmp val (vector-ref v i))
			   ((lt) (lp istart i))
			   ((eq) i)
			   ((gt) (lp (inc i) iend))))))))


(def (vector-binsearch/start+end [vector? v] ;; must be sorted using cmp
				 val
				 [function? cmp]
				 [fixnum-natural0? istart]
				 [fixnum-natural0? iend])
     -> (maybe fixnum-natural0?)

     (let ((len (vector-length v)))
       (assert (<= istart len))
       (assert (<= iend len))
       (assert (<= istart iend)))
     (@vector-binsearch/start+end v val cmp istart iend))


(def (vector-binsearch [vector? v] ;; must be sorted using cmp
		       val
		       [function? cmp])
     -> (maybe fixnum-natural0?)

     (@vector-binsearch/start+end v val cmp 0 (vector-length v)))



(def range-of-integer? (range-of integer?))

;; find the first gap in a finite integer sequence, if any
(def (sorted-integers.maybe-gap is) -> (maybe range-of-integer?)
     (if (null? is)
	 #f
	 (let-pair ((a is) is)
		   (let lp ((a a)
			    (is is))
		     (if (null? is)
			 #f
			 (let-pair ((b is) is)
				   (if (> (- b a) 1)
				       (range (+ a 1) b)
				       (lp b is))))))))

(TEST
 > (sorted-integers.maybe-gap '(1 2 3))
 #f
 > (sorted-integers.maybe-gap '(1 3))
 [(range) 2 3]
 > (sorted-integers.maybe-gap '(1 2 4))
 [(range) 3 4]
 > (sorted-integers.maybe-gap '())
 #f
 > (sorted-integers.maybe-gap '(3 3 4 4 6 6))
 [(range) 5 6]
 ;; invalid but well..
 > (sorted-integers.maybe-gap '(3 2))
 #f)


;; For all sets of values having a cmp, when sorted using cmp into a
;; vector and picking out a random value, that value must be found,
;; "and its position be the correct one", and for any value *not* in
;; the set it must return #f. Values not in the set includes both
;; values within the same min max boundaries and those outside.

(TEST
 ;; with no values, any value is outside it
 > (vector-binsearch (vector) 123 number-cmp)
 #f
 > (def (t nvals)
	(let* ((range (inc (random-integer 1000)))
	       (shift (- (random-integer 1300) 800))
	       (*ri (& (- (random-integer range) shift)))
	       (knowncontained (*ri))
	       (vs (repeatedly nvals (C cons (*ri) _) (list knowncontained)))
	       (len (inc nvals))
	       (vs*-list (cmp-sort vs number-cmp))
	       (vs* (list->vector vs*-list)))
	  ;; vs* always contains at least 1 value, knowncontained
	  (let ((vmin (vector.first vs*))
		(vmax (vector.last vs*)))
	    (let ((maybe-not-contained
		   (if-let ((r (sorted-integers.maybe-gap vs*-list)))
			   (range.from r)
			   #f)))
	      (local-TEST
	       > (fixnum-natural0? (vector-binsearch vs* knowncontained number-cmp))
	       #t
	       > (if maybe-not-contained
		     (vector-binsearch vs* maybe-not-contained number-cmp)
		     #f)
	       #f
	       ;; > (vector-binsearch vs* vmin number-cmp)
	       ;; 0
	       ;; ^ fails, makes sense when coming from the center.
	       > (= (vector-ref vs* (vector-binsearch vs* vmin number-cmp)) vmin)
	       #t
	       > (= (vector-ref vs* (vector-binsearch vs* vmax number-cmp)) vmax)
	       #t)
	      (repeat 30
		      (local-TEST
		       > (let ((ri (*ri)))
			   (if-let ((i (vector-binsearch vs* ri number-cmp)))
				   (= (vector-ref vs* i) ri)
				   #t))
		       #t
		       ;; and assured retrieval
		       > (let* ((i (random-integer len))
				(ri (vector-ref vs* i)))
			   (= (vector-ref vs* (vector-binsearch vs* ri number-cmp)) ri))
		       #t))))))
 > (repeat 20 (t (random-integer 1000))))

