;;; Copyright 2013-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         dsssl
	 test
	 (math/vectorlib-1 view)
	 (oo-lib-u32vector u32vector-inc!))

(export show-fn
	view-fn plot

        u32vector:histogram

        u32vector.blocky-function
        blocky-histogram
        plot-blocky-histogram
        
	u32vector.smooth-function
	smooth-histogram
        plot-histogram

        #!optional
	min+max-values
	min+max-values-values)

(include "../../cj-standarddeclares.scm")

(possibly-use-debuggable-promise)


(define (min+max-values v mima)
  (letv ((mi ma) mima)
	(values (min v mi)
		(max v ma))))

(define (min+max-values-values vv mima)
  (min+max-values (snd vv) (min+max-values (fst vv) mima)))

(define (show-fnS fnS x0 x1
                  #!key
                  y0
                  y1
                  ;; size (dimension of the square image)
                  (s 600)
                  ;; how many points to draw for each pixel-step in x
                  ;; direction
                  (oversampling 10))
  (let* ( ;; Marix of pixels representing the image: (xx Mi:zeros would
	 ;; make sense since I'm calculating with integers)
         (m (Mr:zeros s s))
         ;; size in function points
	 (s* (* oversampling s))
	 (i.x (lambda (i)
		(+ x0 (* (/ i s*) (- x1 x0)))))
	 (x.i (lambda (x)
		(* (/ (- x x0) (- x1 x0)) s*)))
         ;; List of (vector with the y values) per function:
	 (vss (mapS
	       (lambda (fn)
		 (let ((vs (@make-Vr s*)))
		   (for..< (i 0 s*)
			   (let* ((x (i.x i))
				  (y (fn x)))
			     (Vr.set! vs i (exact->inexact y))))
		   vs))
	       fnS))
	 
         (*ymin+ymax
          (delay
            (let (mimas (map .min+max vss))
              (fold min+max-values-values (car mimas) (cdr mimas))))))
    (let ((y0 (or y0 (fst (force *ymin+ymax))))
          (y1 (or y1 (snd (force *ymin+ymax)))))
      (when (= y0 y1)
            (error "show.fnS: y0 and y1 are both" y0))
      (warn "y0,y1=" y0 y1)
      (let* ((yrange (- y1 y0))
             (yrange* (* yrange (/ (+ 3 s) s))) ;; 3?
             (ycenter (+ y0 (/ yrange 2)))
             (y0 (- ycenter (/ yrange* 2)))
             (y1 (+ ycenter (/ yrange* 2))))
        (warn "y0,y1=" y0 y1)
        (let ((plot-s* ;; x,y in 0..s* range
               (lambda (i j)
                 ;; how much to add to the points left, right, top, bottom?
                 (letv ((i* irest) (quotient+modulo i oversampling))
                       (letv ((j* jrest) (quotient+modulo j oversampling))
                             (let ((left (- oversampling irest))
                                   (right irest)
                                   (top jrest)
                                   (bottom (- oversampling jrest)))
                               (let ((upd
                                      (lambda (i* j* val)
                                        (when (and (not (zero? val))
                                                   ;; (< i* s)
                                                   (< j* s))
                                              (Mr-update!-inline
                                               m j* i*
                                               (C + _ val))))))
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
                       (* (- 1 (/ (- y y0)
                                  (- y1 y0)))
                          s
                          oversampling))))))
          (for..< (i 0 (- s* oversampling))
                  ;;^ - since updating 'right' will always hit the
                  ;;right edge, too, even if there's no pixel there?
                  ;;hm notsuretho,stillbuggy?
                  (for-each (lambda (vs)
                              (plot-s* i (y.j (Vr.ref vs i))))
                            vss))
          ;; x axis
          (let ((j (y.j 0)))
            (when (and (<= 0 j) (< j s*))
                  (for..< (i 0 (dec s))
                          (plot-s* (* i oversampling) j))))
          ;; y axis
          (let ((i (x.i 0)))
            (when (and (<= 0 i) (< i s*))
                  (for..< (j 0 (dec s))
                          (plot-s* (integer i)
                                   ;; XX hm. ^ integer just so that
                                   ;; quotient+modulo doesn't croak
                                   (* j oversampling))))))))
    m))


(define show-fn show-fnS)

(define view-fn
  (compose* view
	    (C .map _ sqrt)
	    show-fn))

(define plot view-fn)


;; --- Utils ------------------------------------------------------------
;; Do these belong here? (Or a place like math/integral? Mess)

(def (u32vector:histogram xs ;; #((list-of real?) xs) ;; stream-min&max checks too
			  #!optional
			  (num-buckets 100))
     (let* ((v (make-u32vector num-buckets))
	    (imax (dec num-buckets))
	    (xminmax (stream-min&max xs))
	    (xmin (fst xminmax))
	    (xmax (snd xminmax))
	    (d (- xmax xmin))
	    (slot (lambda (x)
		    (let ((i (integer (* (/ (- x xmin) d) num-buckets))))
		      ;; XX *can* it happen that we land outside the
		      ;; range?
		      (if (< i 0)
			  0
			  (if (>= i num-buckets)
			      imax
			      i))))))
       (for-each (lambda (x)
		   (u32vector-inc! v (slot x)))
		 xs)
       v))

(TEST
 > (u32vector:histogram '(10 30) 3)
 #u32(1 0 1)
 > (u32vector:histogram '(10 30 28) 3)
 #u32(1 0 2)
 > (u32vector:histogram '(28 10 30) 3)
 #u32(1 0 2)
 > (u32vector:histogram '(28 10 30 -5) 3)
 #u32(1 1 2)
 > (u32vector:histogram '(28 10 30 -5 1 2) 3)
 #u32(3 1 2)
 )

;; provide a function taking 0..1 for the position in the vector and
;; returning the value in the vector (XX with linear(?)
;; interpolation). I *had* this some where!
;; Use OO and a |vector*?| ? Or generate in vector-util ?

(def. (u32vector.blocky-function v)
  (let* ((len (u32vector-length v))
	 ;; heh copy-paste? no not completely, x is 0..1 here
	 (slot (lambda (x)
		 (let ((i (integer (+ 0.5 (* x len)))))
		   ;; XX *can* it happen that we land outside the
		   ;; range?
		   (if (< i 0)
		       0
		       (if (>= i len)
			   (dec len)
			   i))))))
    (lambda (x)
      (u32vector-ref v (slot x)))))

(def (histogram/u32vector-to-function to-fn)
     (lambda (xs
         #!optional
         (num-buckets 100)
         pp?)
       (to-fn ((if pp? pp-through identity) (u32vector:histogram xs num-buckets)))))

(def blocky-histogram (histogram/u32vector-to-function .blocky-function))

(TEST
 > (def f (blocky-histogram '(28 10 30 -5 1 2) 3))
 > (f 0)
 3
 > (f 0.1)
 3
 > (f 0.2)
 1
 > (f 0.8)
 2)

(def (plot-blocky-histogram xs
                            #!optional
                            (num-buckets 100))
     (plot (blocky-histogram xs num-buckets) 0 1))


(def. (u32vector.smooth-function vec)
  ;; Some sort of weighted average.
  ;; weight = 1/ distance^2
  ;; When too close, just take the value.

  ;; ! corresponding to = in C (or to ! in Erlang, somewhat :)
  (define-macro (! var v)
    `(##f64vector-set! ,var 0 ,v))
  (define-macro (ref var)
    `(##f64vector-ref ,var 0))
  (define-macro (+! var v)
    `(! ,var (fl+ (ref ,var) ,v)))
  (define-macro (square v)
    `(fl* ,v ,v))
  
  (let* ((len (u32vector-length vec))

         ;; Conversion to i's scale
         (x-scaler (exact->inexact (dec len)))

         (tot (f64vector 0.))
         (tot-weight (f64vector 0.)))
      
    (lambda ([flonum? x])
      ;; x \in 0..1
      ;; Zoom out a bit so that the edge values can be seen fully:
      (declare (not safe))
      (let* ((x-scaled (fl* x x-scaler)))
        (! tot 0.)
        (! tot-weight 0.)
        (for..< (i 0 len)
                (let* ((dist (fl- x-scaled (exact->inexact i)))
                       ;; Interesting, (abs dist) is spiky, (square
                       ;; dist) is nice but over-reacting, abs ^3 and
                       ;; ^4 are 'rounded-blocky' as is probably best
                       ;; for this.
                       (dist* (square (square dist)))
                       (weight (fl/ dist*)))
                  ;; (when (> weight 1000000.)
                  ;;       (warn "weight=" weight))
                  (+! tot-weight weight)
                  (+! tot (fl* weight
                               (exact->inexact
                                (u32vector-ref vec i))))))
        (let (w (ref tot-weight))
          (if (or (infinite? w) (nan? w) (fl> w 1e7)) ;; ?
              ;; too close, just take the value.
              (inexact
               (u32vector-ref vec (integer (fl+ x-scaled 0.5))))
              (fl/ (ref tot) w)))))))

(TEST
 > (def vs '(1432777512
             1432820696
             1432832312
             1432852600
             1432889448
             1433025592
             1433119608
             1433128344
             1433151688
             1433158264
             1433169640
             1433179736
             1433179928
             1433231480
             1433308744
             1433347464
             1433368568
             1433525176))
 > (def h (u32vector:histogram vs 6))
 > h
 #u32(5 1 2 6 3 1)
 > (def f (.smooth-function h))
 > (f 0.)
 5.
 > (f (/ 1. 5))
 1.
 > (f (/ 2. 5))
 2.
 > (f (/ 4. 5))
 3.
 > (f (/ 5. 5))
 1.
 > (f 0.5)
 3.974070249946426)


(def smooth-histogram (histogram/u32vector-to-function .smooth-function))

(def (plot-histogram xs
                     #!rest
                     keyword-options)
     "plot a histogram of the input data. Takes num-buckets as a
keyword argument (default: sqrt of the length of xs), and passes other
keyword arguments on to |plot|."
     (let* ((num-buckets
             (force (dsssl-ref keyword-options num-buckets:
                               (delay (integer (sqrt (length xs)))))))
            (half-step (/ 0.5 num-buckets)))
       (apply plot
              (smooth-histogram xs num-buckets #t)
              (- 0. half-step)
              (+ 1. half-step)
              y0: 0
              (=> keyword-options
                  (dsssl-delete '(num-buckets:))
                  (dsssl-defaults '(oversampling: 30))))))
