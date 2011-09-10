;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; (requires lib.lazy)

(define (F s)
  (let F ((s s))
    (let ((s (force s)))
      (if (pair? s)
	  (cons (F (car s))
		(F (cdr s)))
	  s))))

(define (F* s)
  (let F ((s s))
    (cond ((promise? s)
	   (vector '<P>
		   (let ((s (force1 s)))
		     (F s))))
	  ((pair? s)
	   (cons (F (car s))
		 (F (cdr s))))
	  (else
	   s))))

