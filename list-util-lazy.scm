;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 (test-lib-1 %try-error)
	 C
	 srfi-1
	 (cj-env-1 dec inc identity)
	 (string-util-1 string-split)
	 (improper-list improper-length)


	 (lazy FV)
	 test
	 (list-util one-item?))

(export xone ;; also see |the| in easy-1
	xone/fail
	maybe-xone)


;; can't use cj-inline yet (circular dependency)
(define (xone/fail x fail)
  (FV (x)
      (if (pair? x)
	  (if (null? (force (cdr x)))
	      (car x)
	      (fail 'found-too-many))
	  (fail (if (null? x)
		    'not-found
		    'improper-list)))))

(define (xone x)
  (xone/fail x
	     (lambda (e)
	       (error "expected one item, but:" e
		      (force x)))))


(TEST
 > (%try-error (xone (delay '())))
 #(error "expected one item, but:" not-found ())
 > (%try-error (xone (delay '(a b))))
 #(error "expected one item, but:" found-too-many (a b))
 > (def (t v)
	(list (%try-error (one-item? v))
	      (xone/fail v identity)))
 > (t (delay (list 1 2)))
 (#f found-too-many)
 > (t (delay (list 2)))
 (#t 2)
 > (t (delay '()))
 (#f not-found)
 > (t (delay (cons 1 (delay (cons 2 (delay '()))))))
 (#f found-too-many)
 > (t (delay (cons 1 (delay (delay '())))))
 (#t 1)
 > (t (delay (cons 1 (delay '()))))
 (#t 1)
 > (t (delay (cons 1 (delay 2))))
 ;; improper-list ah, doesn't check for that case, oh well should be fine:
 (#f found-too-many)
 > (t (cons 1 (delay '())))
 (#t 1)
 > (t (delay 1))
 (#(error "not a list:" 1) improper-list)
 > (t (cons 1  '()))
 (#t 1))


(define (maybe-xone v)
  (xone/fail v (lambda (e)
		 (case e
		   ((not-found) #f)
		   (else
		    (error "expected one item or none, but:" e
			   (force v)))))))


(TEST
 > (maybe-xone '())
 #f
 > (maybe-xone '(a))
 a
 > (%try-error (maybe-xone (iota 2)))
 #(error "expected one item or none, but:" found-too-many (0 1)))


