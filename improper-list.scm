;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

;; generate from srfi-1... again


(require test
	 (list-util-1 improper-map) ;; can this be moved here?
	 ;; implementation in improper-length--include.scm, included in
	 ;; cj-source-util.scm:
	 ;;(cj-source-util improper-length)
	 )

(export improper-list->list
	improper-map
	improper-fold
	improper-fold-right
	improper-fold-right*
	improper-find
	improper-append
	improper-last
	improper-for-each
	improper-length
	improper-any)


;;; a map accepting improper lists (i.e. including non-pairs as l)

;; implementation see list-util-1.scm

(TEST
 > (improper-map inc-function '(1 2 3))
 (2 3 4)
 > (improper-map inc-function '(1 2 . 3))
 (2 3 . 4)
 > (improper-map inc-function '5)
 6
 > (improper-map inc-function '())
 ()
 > (mapS inc-function '(1 2 . 3))
 (2 3 4)
 > (mapS inc-function '5)
 (6)
 > (mapS inc-function '())
 ()
 > (mapS inc-function 5 'tail)
 (6 . tail)
 )

(define (improper-fold fn tail l)
  (let lp ((tail tail)
	   (l l))
    (cond ((null? l)
	   tail)
	  ((pair? l)
	   (lp (fn (car l) tail)
	       (cdr l)))
	  (else
	   (fn l tail)))))

(TEST
 > (define (inccons n l)
     (cons (inc n)
	   l))
 > (improper-fold inccons 'end '(1 2 3))
 (4 3 2 . end)
 > (improper-fold inccons 'end '(1 2 . 3))
 (4 3 2 . end)
 )

(define (improper-fold-right fn tail l)
  (let rec ((l l))
    (cond ((null? l)
	   tail)
	  ((pair? l)
	   (fn (car l)
	       (rec (cdr l))))
	  (else
	   (fn l tail)))))

(TEST
 > (improper-fold-right inccons 'end '(1 2 3))
 (2 3 4 . end)
 > (improper-fold-right inccons 'end '(1 2 . 3))
 (2 3 4 . end)
 )


(define (improper-list->list l #!optional (tail '()))
  (improper-fold-right cons tail l))

(TEST
 > (improper-list->list '(1 2 . 3))
 (1 2 3)
 > (improper-list->list '(1 2 . 3) 'end)
 (1 2 3 . end))


;; improper-fold-right that also tells whether the argument is the end
;; of an improper input list:

(define (improper-fold-right* fn/3 tail l)
  (let rec ((l l))
    (cond ((null? l)
	   tail)
	  ((pair? l)
	   (fn/3 #f
		 (car l)
		 (rec (cdr l))))
	  (else
	   (fn/3 #t
		 l
		 tail)))))

;; test see improper-fold-right/yn-cont in list-util-2.scm


;; should really use Maybe.scm...
(define (improper-find pred v)
  (let rec ((v v))
    (cond ((null? v)
	   #f)
	  ((pair? v)
	   (if (pred (car v))
	       (car v)
	       (rec (cdr v))))
	  (else
	   (if (pred v)
	       v
	       #f)))))

(TEST
 > (find even? '(1 2 3))
 2
 > (find even? '(1 3))
 #f
 > (improper-find even? '(1 2 3))
 2
 > (improper-find even? '(1 3))
 #f
 > (improper-find even? 3)
 #f
 > (improper-find even? 2)
 2)


(define (improper-append a b)
  (improper-fold-right cons b a))

(TEST
 > (improper-append 'a '(b c d))
 (a b c d)
 > (improper-append '() '(b c d))
 (b c d)
 > (improper-append '(X Y) '(b c d))
 (X Y b c d))


(define (improper-last v)
  (if (pair? v)
      (let ((v* (cdr v)))
	(if (null? v*)
	    (car v)
	    (improper-last v*)))
      v))

(TEST
 > (improper-last 'a)
 a
 > (improper-last '(a))
 a
 > (improper-last '(a b))
 b
 > (improper-last '(a . b))
 b
 > (improper-last '())
 ()
 )


(define (improper-for-each proc v)
  (let lp ((v v))
    (cond ((pair? v)
	   (proc (car v))
	   (lp (cdr v)))
	  ((null? v)
	   (void))
	  (else
	   (proc v)
	   (void)))))

(TEST
 > (define z 0)
 > (define (a n)
     (set! z (+ z n)))
 > (improper-for-each a '(1 2 . 3))
 > z
 6
 > (improper-for-each a 4)
 > z
 10
 )



(define (improper-any pred v)
  (cond ((pair? v)
	 (or (pred (car v))
	     (improper-any pred (cdr v))))
	((null? v)
	 #f)
	(else
	 (pred v))))

(TEST
 > (improper-any identity '(#f 1 #f))
 1
 > (improper-any identity '2)
 2
 > (improper-any even? 1)
 #f
 > (improper-any even? 2)
 #t
 > (improper-any even? '())
 #f)

