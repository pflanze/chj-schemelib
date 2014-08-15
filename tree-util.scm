;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test)

(def (flatten/ drop-false?)
     (named flatten
	    (lambda (v tail)
	      (cond ((pair? v)
		     (flatten (car v)
			      (flatten (cdr v) tail)))
		    ((and drop-false? (not v))
		     tail)
		    ((null? v)
		     tail)
		    (else
		     (cons v tail))))))

(def (flatten v #!optional (tail '()))
     ((flatten/ #f) v tail))

(def (flatten* v #!optional (tail '()))
     ((flatten/ #t) v tail))

(TEST
 > (flatten '())
 ()
 > (flatten '(a b) 'end)
 (a b . end)
 > (flatten '(a . b) '(c))
 (a b c)
 > (flatten '(a ((b . c) d) . e))
 (a b c d e)
 > (flatten '(a ((#f . c) d) . e))
 (a #f c d e)
 > (flatten* '(a ((#f . c) d) . e))
 (a c d e))

