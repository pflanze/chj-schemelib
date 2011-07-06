;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

; requires srfi-1 fold-right

(define (list-join lis val)
  (if (null? lis)
      lis
      (cons (car lis)
	    (fold-right (lambda (v l)
			  (cons val (cons v l)))
			'()
			(cdr lis)))))

(define (objects->string objs
			 #!key
			 (prepend #f) ;; maybe type, *or* boolean
			 (separator " "))
  (apply string-append
	 (let ((m (list-join
		   (map object->string
			objs)
		   separator)))
	   (if (and prepend (pair? objs))
	       (cons (if (string? prepend) prepend separator) m)
	       m))))

