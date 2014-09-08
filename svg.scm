;;; Copyright 2013-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require 2d-shape)


(def. (real.svg-string x)
  (number.string
   (if (integer? x)
       x
       (exact->inexact x))))


(def. (2d-point.svg-fragment shape fit)
  (let ((p (fit shape)))
    `(circle (@ (cx ,(.svg-string (.x p)))
		(cy ,(.svg-string (.y p)))
		(r 1.5)
		(stroke "blue")
		(stroke-width 0)
		(fill "blue")))))


(def (_svg-point command p)
     (list command
	   " "
	   (.svg-string (.x p))
	   " "
	   (.svg-string (.y p))
	   " "))

(def. (2d-line.svg-fragment shape fit)
  (let-2d-line
   ((from to) shape)
   `(path (@ (d ,(cons (_svg-point "M" (fit from))
		       (_svg-point "L" (fit to))))
	     (stroke "black")
	     (stroke-width 1)
	     (fill "none")))))

(def (_svg-circularize command0 command1 ps)
     (let-pair ((p0 ps*) ps)
	       (cons (_svg-point command0 p0)
		     (map/tail (C _svg-point command1 _)
			       (list
				(_svg-point command1 p0))
			       ps*))))

(def. (2d-path.svg-fragment shape fit)
  (let ((ps (map fit (.points shape))))
    (let-pair
     ((p0 ps*) ps)
     `(path
       (@ (d ,(_svg-circularize "M" "L" ps)

	   ;; (cons (_svg-point "M" p0)
	   ;; 	    (map (C _svg-point "L" _)
	   ;; 		 ps*))
	   )
	  (stroke "black")
	  (stroke-width 1)
	  (fill "green"))))))

(def. (2d-square.svg-fragment shape fit)
  `(path
    (@ (d ,(let* ((ps (.points shape))
		  (p0 (car ps)))
	     (cons (_svg-point "M" (fit p0))
		   (fold-right (lambda (p r)
				 (cons (_svg-point "L" (fit p))
				       r))
			       (_svg-point "L" (fit p0))
			       (cdr ps)))))
       (fill "none")
       (stroke "black")
       (stroke-width "1"))))


(def svg-width 800)
(def svg-height 800)

(def (svg size mi ma shapes)
     ;; mi,ma = min and max
     (let* ((fit (let* ((range (.- ma mi))
			(stretch (../ size range)))
		   (let-2d-point
		    ((x0 x1) mi)
		    (lambda (p)
		      (let-2d-point ((p0 p1) p)
				    (..* (.- p mi) stretch)))))))
       `(svg
	 (@ (height ,svg-height)
	    (width ,svg-width))
	 ,(map ;;stream-map
	   (C .svg-fragment _ fit)
	   shapes))))


(def svg-path "out.svg")

(def (showsvg shapes)
     ;; ah want regenerate stream(s) maybe? not cache? well. how to say har.
     (let* ((p0 (.start (car (force shapes)))))
       (let-pair ((mi ma) (stream-fold-left .min+maxs/prev
					    (cons p0 p0)
					    shapes))
		 (sxml>>pretty-xml-file (svg (2d-point svg-width
						       svg-height)
					     mi ma shapes)
					svg-path)
		 (xxsystem "display" "--" svg-path))))
