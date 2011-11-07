;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.srfi-1))


;; This is separate from list-util (and list-util-2) to enable use in
;; mod.scm (bootstrapping)

(define (map/tail func l tail)
  (fold-right (lambda (val tail)
		(cons (func val)
		      tail))
	      tail
	      l))


(define (improper-map fn l #!optional (tail '()))
  (let rec ((l l))
    (cond ((null? l)
	   tail)
	  ((pair? l)
	   (cons (fn (car l))
		 (rec (cdr l))))
	  (else
	   (fn l)))))

;; TEST see list-util.scm


