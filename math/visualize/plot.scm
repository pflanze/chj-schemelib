;;; Copyright 2013-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         dsssl
         math/smoothfn
	 test
	 (math/vectorlib-1 view)
	 (oo-lib-u32vector u32vector-inc!))

(export show-fn
	view-fn plot

        u32vector:histogram

        u32vector.blocky-function
        blocky-histogram
        plot-blocky-histogram
        
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



(def smooth-histogram (histogram/u32vector-to-function .smoothfn))

(def (plot-histogram xs
                     #!rest
                     keyword-options)
     "
Plot a histogram of the input data. Takes the following keyword options:

   num-buckets: number of buckets to use. Default: (expt (length xs)
         num-buckets-exponent))

   num-buckets-exponent: used in the calculation of the default for
         num-buckets. Default: 0.35

Any other keyword options it receives are passed on to |plot|."

     (let* ((num-buckets-exponent
             (dsssl-ref keyword-options num-buckets-exponent:
                        0.35))
            (num-buckets
             (force
              (dsssl-ref keyword-options num-buckets:
                         (delay (integer
                                 (+ 0.5
                                    (expt (length xs)
                                          num-buckets-exponent)))))))
            (half-step (/ 0.5 num-buckets)))
       (apply plot
              (smooth-histogram xs num-buckets #t)
              (- 0. half-step)
              (+ 1. half-step)
              y0: 0
              (=> keyword-options
                  (dsssl-delete '(num-buckets: num-buckets-exponent:))
                  (dsssl-defaults '(oversampling: 30))))))
