;;; Copyright 2014-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test
	 jclass
	 rgb colorspaces)


;; color =================================================================

(jclass (colorstring #(string? value))

	(def-method html-colorstring colorstring.value)

	(def-method maybe-stroke-color identity)
	(def-method maybe-fill-color identity)
	(def-method maybe-stroke-width false/1)


	(def colorstring:inversions
	     (list->table
	      (fold-right (lambda (pair rest)
			    (cons* (cons (car pair)
					 (cadr pair))
				   (cons (cadr pair)
					 (car pair))
				   rest))
			  '()
			  '(("white" "black")
			    ("red" "cyan")
			    ("green" "purple")
			    ("blue" "yellow")
			    ))))
	
	(def-method* (invert v)
	  (let ((v* (string-downcase value)))
	    (cond ((table-ref colorstring:inversions v* #f)
		   => colorstring)
		  (else
		   (error "can't invert color string:" v*))))))


(TEST
 > (.invert (colorstring "White"))
 #((colorstring) "black")
 > (.invert (colorstring "black"))
 #((colorstring) "white")
 > (.invert (colorstring "red"))
 #((colorstring) "cyan")
 )


;; change/parametrize when other solutions abound
(def color? (either colorstring? rgb?))
;; XX jinterface, but then have to change those above!


;; (XX monkey patching)
(def. rgb.maybe-stroke-color identity)
(def. rgb.maybe-fill-color identity)
(def. rgb.maybe-stroke-width false/1)


;; paintoptions ==========================================================

(jclass (colors #(color? stroke)
		#(color? fill))

	(def-method maybe-stroke-color colors.stroke)
	(def-method maybe-fill-color colors.fill)
	(def-method maybe-stroke-width false/1))

;; with stroke width, and optional keyword parameters
(jclass (paint #!key
	       #((maybe color?) stroke-color)
	       #((maybe color?) fill-color)
	       #((maybe positive-real?) stroke-width))

	(def-method maybe-stroke-color paint.stroke-color)
	(def-method maybe-fill-color paint.fill-color)
	(def-method maybe-stroke-width paint.stroke-width))

(def paintoptions?
     (either color? colors? paint?))

;; applying it ===========================================================

(jclass (painted #((improper-list-of paintoptions?) optionS)
		 value)

	(def-method* (start v)
	  (.start value))

	(def-method* (min+maxs/prev a b)
	  (.min+maxs/prev value b)))

