;;; Copyright 2013-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy 2d-shape color)


(def. (real.svg-string x)
  (number.string
   (if (integer? x)
       x
       (exact->inexact x))))


(def default-2d-point-colors (colors (colorstring "blue")
				     (colorstring "blue")))

(def. (2d-point.svg-fragment shape fit #!optional colors)
  (let ((p (fit shape)))
    (let-colors
     ((stroke fill) (or colors default-2d-point-colors))
     `(circle (@ (cx ,(.svg-string (.x p)))
		 (cy ,(.svg-string (.y p)))
		 (r 1.5)
		 (stroke ,(.html-colorstring stroke))
		 (stroke-width 0)
		 (fill ,(.html-colorstring fill)))))))


(def (_svg-point command p)
     (list command
	   " "
	   (.svg-string (.x p))
	   " "
	   (.svg-string (.y p))
	   " "))

(def default-2d-line-color (colorstring "black"))

(def. (2d-line.svg-fragment shape fit #!optional color)
  (let-2d-line
   ((from to) shape)
   `(path (@ (d ,(cons (_svg-point "M" (fit from))
		       (_svg-point "L" (fit to))))
	     (stroke ,(.html-colorstring (or color default-2d-line-color)))
	     (stroke-width 1)))))

(def (_svg-circularize command0 command1 ps)
     (let-pair ((p0 ps*) ps)
	       (cons (_svg-point command0 p0)
		     (map/tail (C _svg-point command1 _)
			       (list
				(_svg-point command1 p0))
			       ps*))))


(def default-2d-path-colors (colors (colorstring "black")
				    (colorstring "green")))

(def. (2d-path.svg-fragment shape fit #!optional colors)
  (let ((ps (map fit (.points shape))))
    (let-pair
     ((p0 ps*) ps)
     (let-colors
      ((stroke fill) (or colors default-2d-path-colors))
      `(path
	(@ (d ,(_svg-circularize "M" "L" ps)

	      ;; (cons (_svg-point "M" p0)
	      ;; 	    (map (C _svg-point "L" _)
	      ;; 		 ps*))
	      )
	   (stroke ,(.html-colorstring stroke))
	   (stroke-width 1)
	   (fill ,(.html-colorstring fill))))))))


(def default-2d-square-colors (colors (colorstring "black")
				      (colorstring "none")))

(def. (2d-square.svg-fragment shape fit #!optional colors)
  (let-colors
   ((stroke fill) (or colors default-2d-square-colors))
   `(path
     (@ (d ,(let* ((ps (.points shape))
		   (p0 (car ps)))
	      (cons (_svg-point "M" (fit p0))
		    (fold-right (lambda (p r)
				  (cons (_svg-point "L" (fit p))
					r))
				(_svg-point "L" (fit p0))
				(cdr ps)))))
	(stroke ,(.html-colorstring stroke))
	(stroke-width "1")
	(fill ,(.html-colorstring fill))))))


(def svg-width 800)
(def svg-height 800)

(def (svg #(2d-point? size)
	  #(2d-window? window) ;; 2d-window into the shapes data
	  shapes ;; flat list of shapes; no grouping supported (yet?)
	  #!key
	  #((maybe color?) background-color)
	  )
     (let* ((fit
	     (let. ((mi range) window)
		   (let* ((stretch (../ size range)))
		     (lambda (p)
		       (..* (.- p mi) stretch))))))
       `(svg
	 (@ (xmlns "http://www.w3.org/2000/svg")
	    (xmlns:xlink "http://www.w3.org/1999/xlink")
	    (height ,svg-height)
	    (width ,svg-width)
	    ,(and background-color
		  `(style ,(string-append
			    "background-color: "
			    (.html-colorstring background-color)
			    ";"))))
	 ;; display from imagemagick 8:6.7.7.10-5 ignores any style
	 ;; etc. attributes of the svg element that set the bgcolor,
	 ;; thus:
	 ,(and background-color
	       `(rect (@ (width "100%")
			 (height "100%")
			 (fill ,(.html-colorstring background-color)))))
	 ,(map ;;stream-map
	   (lambda (shape)
	     (if (colored? shape)
		 (let-colored ((color shape) shape)
			      (.svg-fragment shape fit color))
		 (.svg-fragment shape fit)))
	   shapes))))


(def svg-path "out.svg")

(def (showsvg shapes #!optional keep-proportions? #!rest options)
     ;; ah want regenerate stream(s) maybe? not cache? well. how to say har.
     (let* ((p0 (.start (car (force shapes)))))
       (let-pair ((mi ma) (stream-fold-left .min+maxs/prev
					    (cons p0 p0)
					    shapes))
		 (sxml>>pretty-xml-file
		  (apply svg
			 (2d-point svg-width
				   svg-height)
			 ((if keep-proportions?
			      (C .fit-to-proportions _ 1 #f)
			      identity)
			  (2d-window mi ma))
			 shapes
			 options)
		  svg-path)
		 (future (xxsystem "display" "--" svg-path)))))
