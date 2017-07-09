;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 parallel
	 math/image/pnmatrix-compiled
	 math/image/pnm)

(include "../../cj-standarddeclares.scm")

(define. (ppm8.decolorize m)
  (letv ((s0 s1) (.sizes m))
	(let ((res (make-pgm8 "decolorize" s0 s1)))
	  (parallel-for-all
	   100
	   m
	   (s0 s1)
	   (i0 i1)
	   (ppm8.ref@ m i0 i1
		      (lambda (r g b)
			;;XXX correct gamma!....
			(pgm8.set!@ res i0 i1 (quotient (fx+ r g b) 3)))))
	  res)))



(define. (pgm8.invert! m)
  ;;^ ewig type check issue.fix that in define. finally.something.
  (parallel-for-all
   100 m (s0 s1) (i0 i1)
   (pgm8.ref@ m i0 i1
	      (lambda (v)
		(pgm8.set!@ m i0 i1
			    (- 255 v))))))
;; still calculates position twice, stupid...wll.  optimize? ? ?

;; for(i0=0;i<s0;i0++) {
;;    for (i1=0;i1<s1;i1++) { m[i0,i1] = 255 - m[i0,i1]; }}

