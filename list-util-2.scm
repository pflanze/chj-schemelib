;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.test)
	 (lib.cj-env)
	 (lib.define-nested)
	 (lib.slib-sort))


(define list-util-nothing (gensym 'nothing))

;; formerly called |sortedlist-group-by|
(define (group-by lis equal? #!optional (tail '()))
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
 > (group-by '() =)
 ()
 > (group-by '(1) =)
 ((1))
 > (group-by '(1 2 3 3 4 5 6) =)
 ((1) (2) (3 3) (4) (5) (6))
 > (group-by '(1 2 3 3 4 5 5 5 6) =)
 ((1) (2) (3 3) (4) (5 5 5) (6))
 > (group-by '(1 2 3 3 4 5 5 5 6 6) =)
 ((1) (2) (3 3) (4) (5 5 5) (6 6))
 )

;; formerly called |list-group-by|
(define (segregate lis less? #!optional (tail '()))
  (define (equal? a b)
    (and (not (less? a b))
	 (not (less? b a))))
  (group-by (sort lis less?)
	    equal?
	    tail))

(TEST
 > (segregate '() <)
 ()
 > (segregate '(3 1 2 5 6 3 4 3 5) <)
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


(define (lists-common-prefix-length ls equal?)
  (let lp ((ls ls)
	   (len 0))
    (if (any null? ls)
	len
	(let ((v (car (car ls))))
	  (if (every (lambda (l)
		       (equal? (car l) v))
		     (cdr ls))
	      (lp (map cdr ls) (inc len))
	      len)))))

(TEST
 > (lists-common-prefix-length '(() ()) equal?)
 0
 > (lists-common-prefix-length '((a) ()) equal?)
 0
 > (lists-common-prefix-length '((a) (b)) equal?)
 0
 > (lists-common-prefix-length '((a) (a b)) equal?)
 1
 > (lists-common-prefix-length '((a c) (a b)) equal?)
 1
 > (lists-common-prefix-length '((a c) (a c b)) equal?)
 2
 )

