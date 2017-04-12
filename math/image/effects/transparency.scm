;; fake transparency  for now

(declare (standard-bindings)
	 (extended-bindings)
	 (block))

;; hm  we don't have a map yet hum  hm hum echt?
;;gsrg  for forever   b jo  b  hu f
;; aha hab ja now immerhin das parallel-for-all
;; ach ausser, muss doch zersch schon .sizes ho9len  und kreieren  jst dummlich

;; sgh makro für map rf rg?f


(define. (pgm8.val-replace pic origval newval)
  (letv ((s0 s1) (.sizes pic))
	(let ((res (make-pgm8 "val-replace" s0 s1)))
	  (parallel-for-all
	   2000
	   pic
	   (s0 s1)
	   (i0 i1)

	   (pgm8.ref@ pic i0 i1
		      (lambda (v)
			(pgm8.set!@ res i0 i1
				    (if (fx= v origval) newval v)))))
	  res)))

(define. ppm8.overlay-pgm8
  (typed-lambda (#(ppm8? layer0) #(pgm8? layer1) transparentval)
		(let-values (((s0 s1) (.sizes layer0))
			     ((s0* s1*) (.sizes layer1)))
		  (assert (= s0 s0*))
		  (assert (= s1 s1*))
		  (let ((res (make-ppm8 "overlay-pgm8" s0 s1)))
		    (parallel-for-all
		     1000
		     res
		     (_s0 _s1)
		     (i0 i1)
		     
		     (ppm8.ref@
		      layer0 i0 i1
		      (lambda (r g b)
			(pgm8.ref@
			 layer1 i0 i1
			 (lambda (v)
			   (if (fx= v transparentval)
			       (ppm8.set!@ res i0 i1 r g b)
			       (ppm8.set!@ res i0 i1 v v v)))))))
		    ;;GRRR ALWAYS for get that:
		    res))))

;;--------------------------------------------------------------------------------
;;XXXXX move to pnm lib right?  .
(define. (pgm8.Mr in)
  (letv ((s0 s1) (.sizes in))
	(let ((res (@make-Mr s0 s1)))
	  (for..< (i0 0 s0)
		  (for..< (i1 0 s1)
			  (pgm8.ref in i0 i1
				    (lambda (v)
				      (Mr.set! res i0 i1
					       (fl* .00392156862745098
						    (exact->inexact v)))))))
	  res)))

