;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 ;; test
	 )

(export rgb:0..1?)

(include "cj-standarddeclares.scm")


(def 01-margin 0.001)
(def min01 (- 01-margin))
(def max01 (+ 1 01-margin))


;; Suspected that the straight-forward (<= min01 v max01) approach was
;; very slow, so try to avoid number format conversions (turns out was
;; already getting an inexact anyway, oddly slow, optimize via fl<= as
;; well--doesn't actually help really):

(def min01-exact (inexact->exact min01))
(def max01-exact (inexact->exact max01))

(def (rgb:0..1? v)
     (and (real? v)
	  (cond ((fixnum? v)
		 (or (eq? v 0)
		     (eq? v 1)))
		((exact? v)
		 (<= min01-exact v max01-exact))
		(else
		 (fl<= min01 v max01)))))

