;;; Copyright 2010, 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (test)
	 (lazy)
	 (cj-typed))


;;;
;;;; variants of srfi-1 functions that don't complain if the list ends
;;;; too early
;;;

(define-typed (weak-take lis #(integer? k))
  (let recur ((lis lis)
	      (k k))
    (FV (lis)
	(if (or (zero? k) (null? lis))
	    '()
	    (cons (car lis)
		  (recur (cdr lis) (- k 1)))))))

(define-typed (weak-drop lis #(integer? k))
  (let iter ((lis lis)
	     (k k))
    (if (zero? k)
	lis
	(FV (lis)
	    (if (pair? lis)
		(iter (cdr lis)
		      (- k 1))
		'())))))

(define-typed (weak-split-at x #(natural0? k))
  (let recur ((lis x) (k k))
    (if (zero? k)
	(values '() lis)
	(FV (lis)
	    (if (null? lis)
		(values '() '())
		(receive (prefix suffix) (recur (cdr lis) (- k 1))
			 (values (cons (car lis) prefix) suffix)))))))

(TEST
 > (weak-take (stream-iota 2) 0)
 ()
 > (weak-take (stream-iota 2) 1)
 (0)
 > (weak-take (stream-iota 2) 2)
 (0 1)
 > (weak-take (stream-iota 2) 3)
 (0 1)
 > (weak-take '() 0)
 ()
 > (weak-take '() 1)
 ()
 
 > (weak-drop (iota 5) 4)
 (4)
 > (promise? (weak-drop (stream-iota 5) 4))
 #t
 > (weak-drop (iota 5) 5)
 ()
 > (weak-drop (iota 5) 6)
 ()
 > (weak-drop '() 0)
 ()
 > (weak-drop '() 1)
 ()

 > (values->vector (weak-split-at '(a b c d e) 6))
 #((a b c d e) ())
 > (values->vector (weak-split-at '(a b c d e) 5))
 #((a b c d e) ())
 > (values->vector (weak-split-at '(a b c d e) 4))
 #((a b c d) (e))
 > (values->vector (weak-split-at '() 4))
 #(() ())
 > (values->vector (weak-split-at '() 0))
 #(() ())
 )
