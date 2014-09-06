;;; Copyright 2013-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(declare (standard-bindings)
	 (extended-bindings)
	 (block))


(defstruct point
  #(exact? x)
  #(exact? y))

(define (_point-op op)
  (lambda (a b)
    (let-point ((a0 a1) a)
	       (let-point ((b0 b1) b)
			  (point (op a0 b0)
				 (op a1 b1))))))

(def. point.+ (_point-op +))
(def. point.- (_point-op -))
(def. point..* (_point-op *))
(def. point../ (_point-op /))

(def. (point.rot90 p)
  (let-point ((x y) p)
	     (point (- y) x)))

(TEST
 > (define a (point 10 1))
 > (define b (.rot90 a))
 > (define c (.rot90 b))
 > (define d (.rot90 c))
 > (.rot90 d)
 #(point 10 1))

(def. (point.= a b)
  (let-point ((a0 a1) a)
	     (let-point ((b0 b1) b)
			(and (= a0 b0)
			     (= a1 b1)))))


(def. (point.< a b)
  (let-point ((a0 a1) a)
	     (let-point ((b0 b1) b)
			(or (< a0 b0)
			    (and (not (< b0 a0))
				 (< a1 b1))))))


(def. point.min+maxs/prev
  (lambda (p min+max)
    (let-point
     ((p0 p1) p)
     (let-pair
      ((mi ma) min+max)
      (let-point
       ((mi0 mi1) mi)
       (let-point
	((ma0 ma1) ma)

	(cons (point (min p0 mi0)
		     (min p1 mi1))
	      (point (max p0 ma0)
		     (max p1 ma1)))))))))

(def (points-min+maxs/prev ps min+max)
     (stream-fold-left point.min+maxs/prev
		       min+max
		       ps))

(TEST
 > (points-min+maxs/prev (list (point 10 12) (point 7 11))
			 (cons (point 10 -11)(point 10 -11)))
 (#(point 7 -11) . #(point 10 12)))


(def. (point.start p)
  p)


(def points? (list-of point?))

(def. (points.sort l)
  (sort l point.<))

