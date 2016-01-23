;;; Copyright 2013-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(declare (standard-bindings)
	 (extended-bindings)
	 (block))

(require easy more-oo)


(def (almost= x y max-abs-diff)
     (or (= x y)
	 (< (abs (- x y)) max-abs-diff)))

(def (almost=/max-abs-diff max-abs-diff)
     (cut almost= <> <> max-abs-diff))

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

		 (method (x/y p)
			 (let-2d-point ((x y) p)
				       (/ x y)))

		 (method (rot90 p)
			 (let-2d-point ((x y) p)
				       (2d-point (- y) x)))

		 (method (= a b)
			 (let-2d-point ((a0 a1) a)
				       (let-2d-point ((b0 b1) b)
						     (and (= a0 b0)
							  (= a1 b1)))))

		 (method (almost= a b max-abs-diff)
			 (def almost= (almost=/max-abs-diff max-abs-diff))
			 (let-2d-point ((a0 a1) a)
				       (let-2d-point ((b0 b1) b)
						     (and (almost= a0 b0)
							  (almost= a1 b1)))))

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
			 p)

		 (method (distance^2 p)
			 ;; distance from root
			 (let-2d-point ((x y) p)
				       (+ (square x) (square y))))

		 ;; should this be called magnitude ?
		 (method (distance p)
			 (sqrt (2d-point.distance^2 p)))

		 ;; also see 2d-point.polar (2d-polar.scm)
		 (method (angle p)
			 (let-2d-point ((x y) p)
				       (atan y x))))

       ;; hmm partial COPY-PASTE from above, how to avoid?
       (subclass partial-2d-point
		 (struct #((maybe real?) x)
			 #((maybe real?) y))
		 ;; No requirement that at least one dimension is set?
		 (defenum partial-2d-point-kind
		   full
		   x-given
		   y-given
		   empty)
		 (method (partial-kind v)
			 (let-partial-2d-point
			  ((x y) v)
			  (cond (x (cond (y 'full)
					 (else 'x-given)))
				(y 'y-given)
				(else
				 'empty))))
		 (method (2d-point v)
			 (let-partial-2d-point
			  ((x y) v)
			  (2d-point x y))))

       (subclass 2d-line
		 (struct #(2d-point? from)
			 #(2d-point? to))

		 (method (start v)
			 (2d-line.from v))

		 (method (points v)
			 (list (2d-line.from v)
			       (2d-line.to v)))

		 (method (diff v)
			 (let-2d-line
			  ((from to) v)
			  (2d-point.- to from)))

		 (method (slope v)
			 (let-2d-point
			  ((x y) (2d-line.diff v))
			  (if (zero? x)
			      (if (zero? y)
				  (error "can't calculate slope of line ending in same point as origin"
					 v)
				  (/ y (exact.inexact x)))
			      (/ y x)))))

       (subclass 2d-path
		 (struct #((list-of 2d-point?) points)
			 #!optional
			 #(boolean? closed?))
		 (method (points-add v #(2d-point? p)) ;; prepend, cons. hm.
			 (let-2d-path ((ps closed?) v)
				      (2d-path (cons p ps)
					       closed?)))
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
			  (.- ma mi)))

		 ;; hm, rename this to x/y like the method for
		 ;; 2d-point, or vice versa?
		 (method (proportions v) ;; div by zero for zero dy !
			 (let-2d-window
			  ((mi ma) v)
			  (let-2d-point
			   ((x0 y0) mi)
			   (let-2d-point
			    ((x1 y1) ma)

			    (let* ((dx (- x1 x0))
				   (dy (- y1 y0))
				   (our-dx/dy (/ dx dy)))
			      our-dx/dy)))))
		 (method x/y 2d-window.proportions)

		 (method (fit-to-proportions v #((complement zero?) dx/dy) clip?)
			 (let-2d-window
			  ((mi ma) v)
			  (let-2d-point
			   ((x0 y0) mi)
			   (let-2d-point
			    ((x1 y1) ma)

			    (let* ((dx (- x1 x0))
				   (dy (- y1 y0))
				   (our-dx/dy (/ dx dy))

				   (_fit
				    (lambda (dx dy dx/dy x0 x1 y0 y1 2d-point)
				      (let* ((new-dx (* dy dx/dy))
					     (x-offset (/ (- dx new-dx) 2)))
					(2d-window
					 (2d-point (+ x0 x-offset) y0)
					 (2d-point (- x1 x-offset) y1)))))
				   (fit
				    (lambda (prop)
				      (_fit dx dy prop x0 x1 y0 y1
					    2d-point)))
				   (flip-fit
				    (lambda (prop)
				      (_fit dy dx prop y0 y1 x0 x1
					    (flip 2d-point)))))

			      ;; use abs so that negative proportions
			      ;; will work
			      (let ((too-wide? (> (abs our-dx/dy) (abs dx/dy))))
				(if too-wide?
				    (if clip?
					(fit dx/dy)
					(flip-fit (/ dx/dy)))
				    (if clip?
					(flip-fit (/ dx/dy))
					(fit dx/dy))))))))))

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
 #(2d-point 10 1)
 ;; almost= :
 > (def a (2d-point 1 2))
 > (map (lambda (p)
	  (list (.= a p)
		(.almost= a p 1e-10)))
	(list (2d-point 1 2)
	      (2d-point 1 2.0)
	      (2d-point 1 2.01)
	      (2d-point 1 2.0000000000001)))
 ((#t #t)
  (#t #t)
  (#f #f)
  (#f #t))
 ;; angle: be the same as R5RS's angle
 > (qcheck* '((1. 0.0001)
	      (0.0001 1.)
	      (3.5 0.0001)
	      (0.0001 4.2)
	      (2. 1.)
	      (-1. 2.)
	      (-0.001 300.)
	      (-3 -4)
	      (4 -9))
	    equal?: (almost=/max-abs-diff 1e-10)
	    (applying (compose angle make-rectangular))
	    (applying (compose .angle 2d-point)))
 ()
 > (def f* (compose angle make-rectangular))
 > (def f (compose .angle 2d-point))
 > (with-exception-catcher identity (& (f 0 0)))
 0
 > (f 1 0)
 0
 > (almost= (f 0 1) (/ pi 2) 1e-10)
 #t
 > (almost= (f -1 0) pi 1e-10)
 #t
 > (almost= (f 0 -1) (/ pi -2) 1e-10)
 #t
 )


(defmacro (with-import-2d-aliases longnames . body)
  `(##let ,(source-map (lambda (longname)
			 `(,(symbol.replace-substrings
			     (source-code longname)
			     "2d-" "")
			   ,longname))
		       longnames)
	  ,@body))

(defmacro (with-import-2d* . body)
  `(with-import-2d-aliases
    (2d-point
     2d-line
     2d-path
     2d-window
     ;;2d-square ;;hmm. square is mine, not Scheme's. but still
     canonical-2d-square
     ;; aliasing the predicates would go too far?
     )
    ,@body))


(TEST
 > (def point 2d-point)
 > (def window 2d-window)

 ;; ---- a quadratic window
 > (def w (window (point 10 11) (point 20 21)))
 > (.range w) ;; .size ? well whatever
 #(2d-point 10 10)
 ;; for every w, the division of the numbers in .range equals .proportions
 > (.proportions w)
 1
 ;; -- cut
 > (def w2 (.fit-to-proportions w 2 #t))
 > w2
 #(2d-window #(2d-point 10 27/2) #(2d-point 20 37/2))
 ;; for every w and y, (comp .proportions (C .fit-to-proportions _ x
 ;; y)) equals x
 > (.range w2)
 #(2d-point 10 5)
 > (.proportions w2)
 2
 ;; -- add borders
 > (def w2 (.fit-to-proportions w 2 #f))
 > w2
 #(2d-window #(2d-point 5 11) #(2d-point 25 21))
 > (.range w2)
 #(2d-point 20 10)
 > (.proportions w2)
 2

 ;; ---- a window that's wider than high [and uses a negative coordinate]
 > (def w (window (point -10 11) (point 20 21)))
 > (.proportions w)
 3
 > (def w2 (.fit-to-proportions w 2 #t))
 > w2
 #(2d-window #(2d-point -5 11) #(2d-point 15 21))
 > (.range w2)
 #(2d-point 20 10)
 > (.proportions w2)
 2

 > (def w2 (.fit-to-proportions w 6 #t))
 > w2
 #(2d-window #(2d-point -10 27/2) #(2d-point 20 37/2))
 > (.range w2)
 #(2d-point 30 5)
 > (.proportions w2)
 6

 > (def w2 (.fit-to-proportions w -6 #t))
 > w2
 #(2d-window #(2d-point -10 37/2) #(2d-point 20 27/2))
 ;; yes, it's flipped over now (there are two ways to flip, though,
 ;; why that variant?)
 > (.range w2)
 #(2d-point 30 -5)
 > (.proportions w2)
 -6
 )

;; properties based tests

(TEST
 > (compile-time (def *skip-flipped-windows* #f))
 > (defmacro (*skip-flipped-windows*:with-exit var body)
     (IF *skip-flipped-windows*
	 `(call/cc (lambda (,var) ,body)) ;;`(with-exit ,var ,body)
	 body))
 > (def (predt x0 y0 x1 y1 prop)
	(*skip-flipped-windows*:with-exit
	 stop
	 (with-import-2d*
	  (let*
	      ((w (window (point x0 y0) (point x1 y1)))
	       (cont
		(lambda ()
		  (let ((w2 (.fit-to-proportions w prop #t))
			(w3 (.fit-to-proportions w prop #f))
			(w-prop (.proportions w)))

		    (IF *skip-flipped-windows*
			(let-2d-point ((w h) (.range w))
				      (if (or (negative? w)
					      (negative? h))
					  (stop))))
			  
		    ;; w2 and w3 should usually be different
		    (assert
		     ((if (= w-prop prop) ;; the original is already
			  ;; of the requested props
			  identity
			  not)
		      (equal? w2 w3)))

		    ;; for every w, the division of the numbers
		    ;; in .range equals .proportions
		    (assert (equal? (.proportions w)
				    (let-2d-point ((x y) (.range w))
						  (/ x y))))

		    ;; for every w and y, (comp .proportions (C
		    ;; .fit-to-proportions _ x y)) equals x
		    (assert (equal? (.proportions w2) prop))
		    (assert (equal? (.proportions w3) prop))

		    (let ((inside-outside-prop-proportion^2
			   (/ (.distance^2 (.range w3))
			      (.distance^2 (.range w2)))))
		      (assert (or (equal? inside-outside-prop-proportion^2
					  (square (/ w-prop prop)))
				  ;;XX hack
				  (equal? inside-outside-prop-proportion^2
					  (square (/ prop w-prop))))))))))
	    (let-2d-point
	     ((dx dy) (.range w))

	     (cond ((zero? prop)
		    (with-exceptions-to
		     (lambda (e rethrow)
		       (if (and (error-exception? e)
				(equal?
				 (error-exception-message e)
				 "dx/dy does not match (complement zero?):"))
			   'skip
			   (begin
			     (step)
			     (rethrow e))))
		     cont))

		   ((or (zero? dy) (zero? dx))
		    (with-exceptions-to
		     (lambda (e rethrow)
		       (if (divide-by-zero-exception? e)
			   'skip
			   (rethrow e)))
		     cont))

		   (else (cont))))))))

 ;; explicit non-random cases:
 > (predt 10 10 21 20 2/3)
 > (predt 11 10 20 20 2/3)
 > (predt 9 10 20 20 2/3)
 > (predt 10 10 20 20 1)
 > (predt 10 10 20 20 3/2)
 > (predt 10 10 20 20 2)
 > (predt 10 10 20 20 2/3)

 ;; random:
 > (def 2d-test-count 0)
 > (do-iter 20
	    (lambda (i0)
	      (do-iter (random-natural0 (inc (square i0)))
		       (lambda (i)
			 (inc! 2d-test-count)
			 (predt
			  (random-integer.. -5 20)
			  (random-integer.. -2 5)
			  (random-integer.. -5 19)
			  (random-integer.. -2 6)
			  (random-fraction (+ (square i) 2))))))))


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

(TEST ;; 2d-line.diff and 2d-line.slope
 > (.diff (2d-line (2d-point 1 2) (2d-point 3 4)))
 #(2d-point 2 2)
 > (.slope (2d-line (2d-point 1 2) (2d-point 3 4)))
 1
 > (.diff (2d-line (2d-point 1 2) (2d-point -3 4)))
 #(2d-point -4 2)
 > (.slope (2d-line (2d-point 1 2) (2d-point -3 4)))
 -1/2
 > (.slope (2d-line (2d-point 1 2) (2d-point 1 4)))
 +inf.0
 > (.slope (2d-line (2d-point 1 2) (2d-point 1 -4)))
 -inf.0)


(def 2d-squares? (list-of 2d-square?))

(def. (2d-squares.sort l)
  (sort l 2d-square.<))

(def 2d-points? (list-of 2d-point?))

(def. (2d-points.sort l)
  (sort l 2d-point.<))

