;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.srfi-1))


(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

(define (list-join lis val)
  (if (null? lis)
      lis
      (cons (car lis)
	    (fold-right (lambda (v l)
			  (cons val (cons v l)))
			'()
			(cdr lis)))))

(define (scm:object->string v)
  (parameterize ((current-readtable
		  (readtable-max-write-level-set
		   (readtable-max-write-length-set
		    (current-readtable)
		    12)
		   6)))
		(object->string v)))

(define (scm:objects->string objs
			     #!key
			     (prepend #f) ;; maybe type, *or* boolean
			     (separator " "))
  (apply string-append
	 (let ((m (list-join
		   (map scm:object->string
			objs)
		   separator)))
	   (if (and prepend (pair? objs))
	       (cons (if (string? prepend) prepend separator) m)
	       m))))
