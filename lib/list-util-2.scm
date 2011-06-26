;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


(define list-util-nothing (gensym 'nothing))

(define (sortedlist-group-by lis equal? #!optional (tail '()))
  ;; return list of lists of the items that are equal according to
  ;; equal?; requires lis to be sorted according to equal?.

  (let ((branch
	 (lambda (lis prev-e)
	   (let rec ((lis lis))
	     (if (null? lis)
		 (values '()
			 tail)
		 (let-pair ((e lis*) lis)
			   (if (equal? prev-e e)
			       (letv ((lisL lisR) (rec lis*))
				     (values (cons e lisL)
					     lisR))
			       (values '()
				       lis))))))))
    (let rec ((lis lis)
	      (prev-e list-util-nothing))
      (if (null? lis)
	  tail
	  (let-pair ((e lis*) lis)
		    (letv ((lisL lisR) (branch lis* e))
			  (cons (cons e lisL)
				(rec lisR e))))))))

(TEST
 > (sortedlist-group-by '() =)
 ()
 > (sortedlist-group-by '(1) =)
 ((1))
 > (sortedlist-group-by '(1 2 3 3 4 5 6) =)
 ((1) (2) (3 3) (4) (5) (6))
 > (sortedlist-group-by '(1 2 3 3 4 5 5 5 6) =)
 ((1) (2) (3 3) (4) (5 5 5) (6))
 > (sortedlist-group-by '(1 2 3 3 4 5 5 5 6 6) =)
 ((1) (2) (3 3) (4) (5 5 5) (6 6))
 )

(define (list-group-by lis less? #!optional (tail '()))
  (define (equal? a b)
    (and (not (less? a b))
	 (not (less? b a))))
  (sortedlist-group-by (sort lis less?)
		       equal?
		       tail))

(TEST
 > (list-group-by '() <)
 ()
 > (list-group-by '(3 1 2 5 6 3 4 3 5) <)
 ((1) (2) (3 3 3) (4) (5 5) (6))
 )


;; Maybe I'm rather going to use improper-fold-right* directly?

(define-nested ((improper-fold-right/yn-cont proper improper) fn/2 tail l)
  (letv ((improper? res) (improper-fold-right*
			  (lambda-values (improper? v (rimproper? res))
					 (values (or improper? rimproper?)
						 (fn/2 v res)))
			  (values #f tail)
			  l))
	((if improper? improper proper) res)))

(TEST
 > (define (inccons n l)
     (cons (inc n)
	   l))
 > ((improper-fold-right/yn-cont list vector) inccons 'end '(1 2 3))
 ((2 3 4 . end))
 > ((improper-fold-right/yn-cont list vector) inccons 'end '(1 2 . 3))
 #((2 3 4 . end))
 )
