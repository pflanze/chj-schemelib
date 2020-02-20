;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require srfi-1
	 (fixnum inc dec))

(export rest
	map/tail
	map/iota
	filter/iota
	for-each/iota
	improper-map
        improper->proper-map
	mapS
	r-list-split
	list-split
	map/last?
	reverse-map/tail
	reverse-map
	flatten1)


;; srfi-1 defines first, but not rest (nor head nor tail)
(define rest cdr)


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

(define (filter/iota pred lis)
  (let rec ((lis lis)
	    (i 0))
    (if (null? lis) lis
	(let ((a (car lis))
	      (r (rec (cdr lis) (inc i))))
	  (if (pred (car lis) i)
	      (cons a r)
	      r)))))

;; TEST see list-util.scm

(define (for-each/iota proc lis)
  (let lp ((lis lis)
	   (i 0))
    (if (null? lis) (void)
	(let ((a (car lis))
	      (r (cdr lis)))
	  (proc a i)
	  (lp r (inc i))))))

;; TEST see list-util.scm


;; (define (improper-map fn l #!optional (tail '()))
;;   (let rec ((l l))
;;     (cond ((null? l)
;; 	   tail)
;; 	  ((pair? l)
;; 	   (cons (fn (car l))
;; 		 (rec (cdr l))))
;; 	  (else
;; 	   (fn l)))))
;; Already available from cj-source.scm

(define (improper->proper-map fn l #!optional (tail '()))
  (let rec ((l l))
    (cond ((null? l)
	   tail)
	  ((pair? l)
	   (cons (fn (car l))
		 (rec (cdr l))))
	  (else
	   (cons (fn l) tail)))))

;; TEST see list-util.scm

(define (mapS fn vS #!optional (tail '()))
  (let rec ((l vS))
    (cond ((null? l)
	   tail)
	  ((pair? l)
	   (cons (fn (car l))
		 (rec (cdr l))))
	  (else
	   (cons (fn l) tail)))))

;; TEST see list-util.scm


(define (r-list-split l separator #!optional retain-match? reverse?)
  (let ((separator? (if (procedure? separator)
			separator
			(lambda (v)
			  (equal? v separator)))))
    (let lp ((l l)
	     (cum '())
	     (res '()))
      (define (end-cum)
	(if reverse?
	    (reverse cum)
	    cum))
      (if (null? l)
	  (cons (end-cum) res)
	  (let ((a (car l))
		(r (cdr l)))
	    (if (separator? a)
		(lp r
		    '()
		    (let ((res* (cons (end-cum) res)))
		      (if retain-match?
			  (cons a res*)
			  res*)))
		(lp r
		    (cons a cum)
		    res)))))))

(define (list-split l separator #!optional retain-match?)
  (reverse (r-list-split l separator retain-match? #t)))

;; TEST see list-util.scm


(define (map/last? fn l)
  (let rec ((l l))
    (if (null? l)
	l
	(let ((r (cdr l)))
	  (cons (fn (null? r)
		    (car l))
		(map/last? fn r))))))

;; TEST see list-util.scm


(define (reverse-map/tail fn l tail)
  (let lp ((out tail)
	   (l l))
    (if (null? l)
	out
	(lp (cons (fn (car l))
		  out)
	    (cdr l)))))

(define (reverse-map fn l)
  (reverse-map/tail fn l '()))


(define (flatten1 lis #!optional (tail '()))
  (fold-right (lambda (v l)
		(if (pair? v)
		    (append v l)
		    (cons v l)))
	      tail
	      lis))

