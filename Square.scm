;;; Copyright 2013-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(declare (standard-bindings)
	 (extended-bindings)
	 (block))


(defstruct Square
  #(point? start)
  #(point? vector))

(def. (Square.points s)
  (let-Square
   ((p v) s)
   (let ((v90 (.rot90 v))
	 (p2 (.+ p v)))
     (list p
	   p2
	   (.+ p2 v90)
	   (.+ p v90)))))

(def. (Square.< a b)
  (let-Square
   ((as av) a)
   (let-Square
    ((bs bv) b)
    (if (point.< as bs)
	#t
	(if (point.< bs as)
	    #f
	    (point.< av bv))))))

(define (canonical-square p1 p2)
  ;; The square is built by extending from p2 with rot90.  A canonical
  ;; square has the point with the smallest x[,y] coordinate as its
  ;; start point.
  (let-point
   ((x1 y1) p1)
   (let-point
    ((x2 y2) p2)
    (let* ((d (point.- p2 p1))
	   (d90 (point.rot90 d))
	   (p3 (point.+ p2 d90))
	   (p4 (point.+ p1 d90)))
      (if (< x1 x2)
	  (if (< y1 y2)
	      (Square p4 (point.- p1 p4))
	      (Square p1 (point.- p2 p1)))
	  (if (= x1 x2)
	      (if (<= y1 y2)
		  (Square p4 (point.- p1 p4))
		  (Square p2 (point.- p3 p2)))
	      (if (<= y1 y2)
		  (Square p3 (point.- p4 p3))
		  (Square p2 (point.- p3 p2)))))))))

(TEST
 > (define (t l)
     (all-equal? (shiftmap canonical-square l)))
 > (t '(#(point 9 77) #(point -1 -1) #(point 77 -11) #(point 87 67)))
 #t
 > (t '(#(point 10 1) #(point 10 2) #(point 9 2) #(point 9 1)))
 #t
 ;; not a square:
 > (t '(#(point 10 1) #(point 10 3) #(point 9 2) #(point 9 1)))
 #f

 ;; detail tests (not necessary if the above are successful):
 ;; > (.points (canonical-square (point 10 1) (point 10 2)))
 ;; (#(point 9 1) #(point 10 1) #(point 10 2) #(point 9 2))
 ;; > (.points (canonical-square (point 10 2) (point 9 2)))
 ;; (#(point 9 1) #(point 10 1) #(point 10 2) #(point 9 2))
 ;; > (.points (canonical-square (point 9 2) (point 9 1)))
 ;; (#(point 9 1) #(point 10 1) #(point 10 2) #(point 9 2))
 ;; > (.points (canonical-square (point 9 1) (point 10 1)))
 ;; (#(point 9 1) #(point 10 1) #(point 10 2) #(point 9 2))
 )


(def. (Square.canonical s)
  (let-Square ((s v) s)
	      ;; now stupidly have to add, well
	      (canonical-square s (point.+ s v))))

(def. (Square.min+maxs/prev s min+max)
  (points-min+maxs/prev (Square.points s) min+max))


;;XXX? vs Square.< above ?
(def. (_Square.< a b)
  (if (null? a)
      (begin
	(assert (null? b))
	#f)
      (or (point.< (car a) (car b))
	  (and (not (point.< (car b) (car a)))
	       (_Square.< (cdr a) (cdr b))))))

(def Squares? (list-of Square?))

(def. (Squares.sort l)
  (sort l Square.<))

