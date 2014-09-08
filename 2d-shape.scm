;;; Copyright 2013-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(declare (standard-bindings)
	 (extended-bindings)
	 (block))

(require easy more-oo)

(class 2d-shape
       ;; generic .min+maxs/prev 
       (method (min+maxs/prev v min+max)
			 (fold 2d-point.min+maxs/prev
			       min+max
			       (.points v)))
       
       (subclass 2d-point
		 (struct #(real? x)
			 #(real? y))

		 (def (_point-op op)
		      (lambda (a b)
			(let-2d-point ((a0 a1) a)
				      (let-2d-point ((b0 b1) b)
						    (2d-point (op a0 b0)
							      (op a1 b1))))))
		 (method + (_point-op +))
		 (method - (_point-op -))
		 (method .* (_point-op *))
		 (method ./ (_point-op /))

		 (method (rot90 p)
			 (let-2d-point ((x y) p)
				       (2d-point (- y) x)))

		 (method (= a b)
			 (let-2d-point ((a0 a1) a)
				       (let-2d-point ((b0 b1) b)
						     (and (= a0 b0)
							  (= a1 b1)))))


		 (method (< a b)
			 (let-2d-point ((a0 a1) a)
				       (let-2d-point ((b0 b1) b)
						     (or (< a0 b0)
							 (and (not (< b0 a0))
							      (< a1 b1))))))


		 (method min+maxs/prev
			 (lambda (p min+max)
			   (let-2d-point
			    ((p0 p1) p)
			    (let-pair
			     ((mi ma) min+max)
			     (let-2d-point
			      ((mi0 mi1) mi)
			      (let-2d-point
			       ((ma0 ma1) ma)

			       (cons (2d-point (min p0 mi0)
					       (min p1 mi1))
				     (2d-point (max p0 ma0)
					       (max p1 ma1)))))))))

		 (method (start p)
			 p))

       (subclass 2d-line
		 (struct #(2d-point? from)
			 #(2d-point? to))

		 (method (start v)
			 (2d-line.from v))

		 (method (points v)
			 (list (2d-line.from v)
			       (2d-line.to v))))

       (subclass 2d-path
		 (struct #((list-of 2d-point?) points))

		 (method (start v)
			 (car (2d-path.points v))))

       (subclass 2d-window ;; an untilted rectangle
		 (struct #(2d-point? mi)
			 #(2d-point? ma))

		 (method (start v)
			 (2d-window.mi v))

		 (method (points v)
			 (let-2d-rectangle
			  ((mi ma) v)
			  (let-2d-point
			   ((x0 y0) mi)
			   (let-2d-point
			    ((x1 y1) ma)
			    (list mi
				  (2d-point x1 y0)
				  ma
				  (2d-point x0 y1))))))

		 (method (range v)
			 (let-2d-window
			  ((mi ma) v)
			  (.- ma mi))))

       (subclass 2d-square
		 (struct #(2d-point? start)
			 #(2d-point? vector))

		 (method (points s)
			 (let-2d-square
			  ((p v) s)
			  (let ((v90 (.rot90 v))
				(p2 (.+ p v)))
			    (list p
				  p2
				  (.+ p2 v90)
				  (.+ p v90)))))

		 (method (< a b)
			 (let-2d-square
			  ((as av) a)
			  (let-2d-square
			   ((bs bv) b)
			   (if (2d-point.< as bs)
			       #t
			       (if (2d-point.< bs as)
				   #f
				   (2d-point.< av bv))))))
		 ;;XXX? vs the one above ?
		 ;; (method (< a b)
		 ;;   (if (null? a)
		 ;;       (begin
		 ;; 	(assert (null? b))
		 ;; 	#f)
		 ;;       (or (2d-point.< (car a) (car b))
		 ;; 	  (and (not (2d-point.< (car b) (car a)))
		 ;; 	       (2d-square.< (cdr a) (cdr b))))))

	  
		 (method (canonical s)
			 (let-2d-square ((s v) s)
					;; now stupidly have to add, well
					(canonical-2d-square s (2d-point.+ s v))))))




(TEST
 > (define a (2d-point 10 1))
 > (define b (.rot90 a))
 > b
 #(2d-point -1 10)
 > (define c (.rot90 b))
 > (define d (.rot90 c))
 > (.rot90 d)
 #(2d-point 10 1))



(define (canonical-2d-square p1 p2)
  ;; The square is built by extending from p2 with rot90.  A canonical
  ;; square has the point with the smallest x[,y] coordinate as its
  ;; start point.
  (let-2d-point
   ((x1 y1) p1)
   (let-2d-point
    ((x2 y2) p2)
    (let* ((d (2d-point.- p2 p1))
	   (d90 (2d-point.rot90 d))
	   (p3 (2d-point.+ p2 d90))
	   (p4 (2d-point.+ p1 d90)))
      (if (< x1 x2)
	  (if (< y1 y2)
	      (2d-square p4 (2d-point.- p1 p4))
	      (2d-square p1 (2d-point.- p2 p1)))
	  (if (= x1 x2)
	      (if (<= y1 y2)
		  (2d-square p4 (2d-point.- p1 p4))
		  (2d-square p2 (2d-point.- p3 p2)))
	      (if (<= y1 y2)
		  (2d-square p3 (2d-point.- p4 p3))
		  (2d-square p2 (2d-point.- p3 p2)))))))))

(TEST
 > (define (t l)
     (all-equal? (shiftmap canonical-2d-square l)))
 > (t '(#(2d-point 9 77) #(2d-point -1 -1) #(2d-point 77 -11) #(2d-point 87 67)))
 #t
 > (t '(#(2d-point 10 1) #(2d-point 10 2) #(2d-point 9 2) #(2d-point 9 1)))
 #t
 ;; not a square:
 > (t '(#(2d-point 10 1) #(2d-point 10 3) #(2d-point 9 2) #(2d-point 9 1)))
 #f

 ;; detail tests (not necessary if the above are successful):
 > (.points (canonical-2d-square (2d-point 10 1) (2d-point 10 2)))
 (#(2d-point 9 1) #(2d-point 10 1) #(2d-point 10 2) #(2d-point 9 2))
 > (.points (canonical-2d-square (2d-point 10 2) (2d-point 9 2)))
 (#(2d-point 9 1) #(2d-point 10 1) #(2d-point 10 2) #(2d-point 9 2))
 > (.points (canonical-2d-square (2d-point 9 2) (2d-point 9 1)))
 (#(2d-point 9 1) #(2d-point 10 1) #(2d-point 10 2) #(2d-point 9 2))
 > (.points (canonical-2d-square (2d-point 9 1) (2d-point 10 1)))
 (#(2d-point 9 1) #(2d-point 10 1) #(2d-point 10 2) #(2d-point 9 2))
 )




(def 2d-squares? (list-of 2d-square?))

(def. (2d-squares.sort l)
  (sort l 2d-square.<))

(def 2d-points? (list-of 2d-point?))

(def. (2d-points.sort l)
  (sort l 2d-point.<))

