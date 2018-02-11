(require easy
	 test
	 stream)

;; Stupid numerical integration. Both stupid algorithm (simply add up
;; in constant steps), and implementation (calculate all values, as a
;; stream nonetheless, then build a vector map function from it).


(def default-steps (expt 2 13))

;; (jclass (real-window #(real? from)
;; 		     #(real? to)
;; 		     #(natural? steps)))

;; (jclass (function/window #(function? f)
;; 			 #(real-window? window))

;; 	(def-method (vector s)
;; 	  (let ((v (make-f32vector steps)))
;; 	    (for..< (i 0 steps)
;; 		    (f32vector-set! v i
;; 				    (f )))
;; 	    v))

;; 	(def-method (integral s)
;; 	  (function/window XXXX
;; 			   window)))


(def (function-stream f from to #!optional (steps default-steps))
     (let ((step (/ (- to from) steps)))
       ;; may loose (or win!) steps (when using inexact numbers), but
       ;; OTOH won't mis-match the range.
       (let lp ((x from))
	 (delay
	   (if (< x to)
	       (cons (f x) (lp (+ x step)))
	       '())))))

(def (integral-stream f from to #!optional (steps default-steps))
     (let ((dx (/ (- to from) steps)))
       (let lp ((x from)
		(y 0))
	 (delay
	   (if (< x to)
	       (let ((y (+ y (* (f x) dx))))
		 (cons y (lp (+ x dx) y)))
	       '())))))

(TEST
 > (F (integral-stream (lambda (x) 1) 0 1 20))
 (1/20
  1/10
  3/20
  1/5
  1/4
  3/10
  7/20
  2/5
  9/20
  1/2
  11/20
  3/5
  13/20
  7/10
  3/4
  4/5
  17/20
  9/10
  19/20
  1)
 > (F (integral-stream (lambda (x) 1) 0 1 10))
 (1/10 1/5 3/10 2/5 1/2 3/5 7/10 4/5 9/10 1))


;; but, multiple integrals LOOH  oh well

;; then plot a vector? f  or w f  r stream  fcrs  pls


;; TOTALLY stupid now.  waste of stream AND vector. Just to follow
;; universal function interface for |plot|. Should revamp all of this !F
;; who how--well hell there's a ~mathematician  !
(def (stream->function s from to steps)
     (let ((v (make-f32vector steps)))
       (let lp ((i 0)
		(s s))
	 (FV (s)
	     (if (null? s)
		 (assert (= i steps))
		 (let-pair ((y s*) s)
			   (f32vector-set! v i (inexact y))
			   (lp (inc i)
			       s*)))))
       (let* ((d (- to from))
	      (dx (/ d steps)))
	 (lambda (x)
	   (f32vector-ref v (integer (/ (- x from) dx)))))))


(def (integral f from to #!optional (steps default-steps))
     (stream->function (integral-stream f from to steps)
		       from to steps))

(TEST
 > (def f (integral (lambda (x) 1) 0 1))
 > (f 0)
 1.220703125e-4
 > (f 0.999)
 .9990234375
 > (f 0.5)
 .5001220703125)

