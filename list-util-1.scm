;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.srfi-1))


;; This is separate from list-util (and list-util-2) to enable use in
;; mod.scm (bootstrapping)


(define (map/tail fn tail . liss)
  (apply fold-right
	 (lambda (x tail)
	   (cons (fn x) tail))
	 tail
	 liss))

;; TEST see list-util.scm

(define (map/iota fn lis)
  (let rec ((lis lis)
	    (i 0))
    (if (null? lis) lis
	(cons (fn (car lis) i)
	      (rec (cdr lis) (inc i))))))

;; TEST see list-util.scm


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


(define (r-list-split l separator)
  (let ((separator? (if (procedure? separator)
			separator
			(lambda (v)
			  (equal? v separator)))))
    (let lp ((l l)
	     (cum '())
	     (res '()))
      (if (null? l)
	  (cons cum res)
	  (let ((a (car l))
		(r (cdr l)))
	    (if (separator? a)
		(lp r
		    '()
		    (cons cum res))
		(lp r
		    (cons a cum)
		    res)))))))

(define (list-split l separator)
  (reverse (map reverse (r-list-split l separator))))

;; TEST see list-util.scm


