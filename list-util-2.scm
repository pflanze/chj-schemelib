;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (list-util let-pair)
	 (values letv values->vector)
	 (fixnum inc)
	 define-nested
	 slib-sort
	 cj-symbol
	 (predicates-1 true?)
	 test)

(export (macro pop!)
	group-by
	segregate
	segregate*
	improper-fold-right/yn-cont
	lists-common-prefix-length
	cartesian-product
	cartesian-self-product
	all-equal?
	sublist
	
	#!optional
	shiftmap ;; ?
	cartesian-product-2)

(define-macro* (pop! var)
  (with-gensym
   V
   `(let ((,V ,var))
      (set! ,var (cdr ,V))
      (car ,V))))

(TEST
 > (define l '())
 > (push! l 1)
 > (push! l 2)
 > l
 (2 1)
 > (pop! l)
 2
 > (pop! l)
 1
 ;; > (pop! l) non-error exception
 )

(define list-util-nothing (gensym 'nothing))

;; formerly called |sortedlist-group-by|
;; XX oh: see also list-group (and stream-group) in stream.scm
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
 ((1) (2) (3 3 3) (4) (5 5) (6)))


(define (segregate* lis extract less? #!optional (tail '()))
  (define (equal? a b)
    (and (not (less? a b))
	 (not (less? b a))))
  (map (lambda (group)
	 (cons (extract (car group))
	       group))
       (group-by (sort lis (on extract less?))
		 (on extract equal?)
		 tail)))

(TEST
 > (segregate* '() car <)
 ()
 > (segregate* '("bummer" "abba" "apple" "beta" "carotin" "all")
	       (lambda (v) (string-ref v 0)) char<?)
 ((#\a "abba" "apple" "all")
  ;; (retains original order since sort comparison function only
  ;; checks this field!)
  (#\b "bummer" "beta")
  (#\c "carotin")))



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

;; 'cross product'

(define (cartesian-product-2 a orig-b)
  (let rec ((a a)
	    (b orig-b))
    (cond ((null? a)
	   '())
	  ((null? b)
	   (rec (cdr a)
		orig-b))
	  (else
	   (cons (cons (car a)
		       (car b))
		 (rec a
		      (cdr b)))))))

(TEST
 > (cartesian-product-2 '(E F) (cartesian-product-2 '(C D) '((A) (B))))
 ((E C A) (E C B) (E D A) (E D B) (F C A) (F C B) (F D A) (F D B))
 )

(define (cartesian-product . lists)
  (cond ((null? lists)
	 (error "?XXX"))
	((null? (cdr lists))
	 (map list (car lists)))
	(else
	 (cartesian-product-2 (car lists)
			      (apply cartesian-product (cdr lists))))))

(TEST
 > (cartesian-product '(A B) '(C D) '(E F))
 ((A C E) (A C F) (A D E) (A D F) (B C E) (B C F) (B D E) (B D F))
 > (cartesian-product '(A B) '(C D) '(E))
 ((A C E) (A D E) (B C E) (B D E))
 > (cartesian-product '(A B) '(C D))
 ((A C) (A D) (B C) (B D))
 > (cartesian-product '(A B))
 ((A) (B))
 )


;; a cartesian product for self combinations that omit combinations
;; that already appeared in reverse order, and returns pairs instead
;; of lists.
(define (cartesian-self-product a)
  (let rec ((a a)
	    (b a))
    (cond ((null? a)
	   '())
	  ((null? b)
	   (let ((a* (cdr a)))
	     (rec a* a*)))
	  (else
	   (cons (cons (car a)
		       (car b))
		 (rec a
		      (cdr b)))))))

(TEST
 > (cartesian-self-product '())
 ()
 > (cartesian-self-product '(a))
 ((a . a))
 > (cartesian-self-product '(a b))
 ((a . a) (a . b) (b . b))
 > (cartesian-self-product '(a b c))
 ((a . a) (a . b) (a . c) (b . b) (b . c) (c . c)))


(define (shiftmap fn lis #!optional (tail '()))
  (let rec ((a (car lis))
	    (l (cdr lis)))
    (if (null? l)
	(cons (fn a (car lis)) tail)
	(let-pair ((a* l*) l)
		  (cons (fn a a*)
			(rec a* l*))))))

(define (all-equal? l #!optional (equal? equal?))
  ;; XX optimize..
  (every true?
	 (shiftmap equal?
		   l)))


(TEST
 > (shiftmap vector '(1 2 3))
 (#(1 2) #(2 3) #(3 1))
 > (shiftmap vector '(1))
 (#(1 1))
 ;; > (%try-error (shiftmap vector '()))
 ;; *** ERROR IN (console)@9.1 -- (Argument 1) PAIR expected
 ;; (cdr '())
 )


;; including i0, excluding i1
(define (sublist l i0 i1)
  (take (drop l i0)
	(- i1 i0)))

;; (BTW Haskell moves the number argument first, SRFI-1 the list
;; argument.)

(TEST
 > (define l '(a b c d))
 > (sublist l 0 3)
 (a b c)
 > (sublist l 0 4)
 (a b c d)
 > (sublist l 1 4)
 (b c d)
 > (sublist l 1 2)
 (b)
 > (sublist l 3 4)
 (d)
 > (sublist l 3 3)
 ()
 > (%error? (sublist l 4 3))
 #t
 > (%error? (sublist l 4 5))
 #t
 > (%error? (sublist l -1 5))
 #t
 > (%error? (sublist l -1 4))
 #t
 > (%error? (sublist l -1 1))
 #t
 )

