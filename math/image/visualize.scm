
(define (min+max-values v mima)
  (letv ((mi ma) mima)
	(values (min v mi)
		(max v ma))))

(define (min+max-values-values vv mima)
  (min+max-values (snd vv) (min+max-values (fst vv) mima)))

(define (show-fnS fnS x0 x1 #!optional (s 400))
  (let* ((m (Mr:zeros s s)) ;; Mi:zeros would make sense since I'm
	 ;; calculating with integers
	 (spread 10) ;; how many function points to get per image point
	 (s* (* 10 s)) ;; function points
	 (i.x (lambda (i)
		(+ x0 (* (/ i s*) (- x1 x0)))))
	 (x.i (lambda (x)
		(* (/ (- x x0) (- x1 x0)) s*)))
	 (vss (mapS
	       (lambda (fn)
		 (let ((vs (@make-Vr s*)))
		   (for..< (i 0 s*)
			   (let* ((x (i.x i))
				  (y (fn x)))
			     (.set! vs i (exact->inexact y))))
		   vs))
	       fnS))
	 (mimas (map .min+max vss)))
    (letv ((ymin ymax) (fold min+max-values-values (car mimas) (cdr mimas)))
	  (warn "ymin,ymax=" ymin ymax)
	  (let* ((yrange (- ymax ymin))
		 (yrange* (* yrange (/ (+ 3 s) s))) ;; 3?
		 (ycenter (+ ymin (/ yrange 2)))
		 (y0 (- ycenter (/ yrange* 2)))
		 (y1 (+ ycenter (/ yrange* 2))))
	    (warn "y0,y1=" y0 y1)
	    (let ((plot-s* ;; x,y in 0..s* range
		   (lambda (i j)
		     ;; how much to add to the points left, right, top, bottom?
		     (letv ((i* irest) (quotient+modulo i spread))
			   (letv ((j* jrest) (quotient+modulo j spread))
				 (let ((left (- spread irest))
				       (right irest)
				       (top jrest)
				       (bottom (- spread jrest)))
				   (let ((upd
					  (lambda (i* j* val)
					    (if (not (zero? val))
						(if (and ;; (< i* s)
						     (< j* s))
						    (.update! m j* i*
							      (cut + <> val)))))))
				     (upd i* j*
					  (+ left bottom))
				     (upd i* (inc j*)
					  (+ left top))
				     (upd (inc i*) j*
					  (+ right bottom))
				     (upd (inc i*) (inc j*)
					  (+ right top))))))))
		  (y.j (lambda (y) ;; "y->j"
			 (dec ;;?
			  (integer
			   (* (- 1 (/ (- y y0) (- y1 y0))) s spread))))))
	      (for..< (i 0 (- s* spread))
		      ;;^ - since updating 'right' will always hit the
		      ;;right edge, too, even if there's no pixel there?
		      ;;hm notsuretho,stillbuggy?
		      (for-each (lambda (vs)
				  (plot-s* i (y.j (.ref vs i))))
				vss))
	      ;; x axis
	      (let ((j (y.j 0)))
		(if (and (<= 0 j) (< j s*))
		    (for..< (i 0 (dec s))
			    (plot-s* (* i spread) j))))
	      ;; y axis
	      (let ((i (x.i 0)))
		(if (and (<= 0 i) (< i s*))
		    (for..< (j 0 (dec s))
			    (plot-s* (integer i)
				     ;; XX hm. ^ integer just so that
				     ;; quotient+modulo doesn't croak
				     (* j spread))))))))
    m))


(define show-fn show-fnS)

(define view-fn
  (compose* view
	    (C .map _ sqrt)
	    show-fn))

(define plot view-fn)

