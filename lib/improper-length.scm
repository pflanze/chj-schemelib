;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


(define (improper-length v)
  ;; copy from cj-env because of phasing issue
  (define (inc x)
    (+ x 1))
  ;; /copy
  (let lp ((v v)
	   (l 0))
    (cond ((pair? v)
	   (lp (cdr v)
	       (inc l)))
	  ((null? v)
	   l)
	  (else
	   (- (inc l))))))

(TEST
 > (improper-length '())
 0
 > (improper-length '(1))
 1
 > (improper-length '(a b c))
 3
 > (improper-length '(a b . c))
 -3
 > (improper-length '(a . c))
 -2
 > (improper-length 'c)
 -1
 )
