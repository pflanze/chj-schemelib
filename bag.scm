;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A bag is an improper list of values and bags.

(require easy
	 define-strict-and-lazy
	 test
	 (cj-port with-output-to-string) ;; for test and bag->string
	 )

(export bag-of
	bag-length
	bag-fold
	bag-fold-right
	bag-stream-fold-right
	bag-for-each
	bag->reverse-list
	bag->list
	bag->stream
	bag->string
	)


(def (bag-of pred)
     (named bag-of-pred
	    (lambda (v)
	      (cond ((pair? v)
		     (and (bag-of-pred (car v))
			  (bag-of-pred (cdr v))))
		    ((null? v)
		     #t)
		    (else
		     (pred v))))))

(def (bag-length b)
     (let lp ((n 0)
	      (b b))
       (cond ((pair? b)
	      (let-pair ((a b*) b)
			(lp (lp n a)
			    b*)))
	     ((null? b)
	      n)
	     (else
	      (+ n 1)))))


(def (bag-fold b start fn) ;; vs. fn start b  ?
     (cond ((pair? b)
	    (bag-fold (cdr b) (bag-fold (car b) start fn) fn))
	   ((null? b)
	    start)
	   (else
	    (fn b start))))

(def (bag-for-each b proc) ;; vs. proc b ?
     (bag-fold b (void)
	       (lambda (v _acc)
		 (proc v))))


(def (bag->reverse-list b #!optional (tail '()))
     (bag-fold b tail cons))


(define-strict-and-lazy
  bag-fold-right
  bag-stream-fold-right
  (lambda (b start fn) ;; vs. fn start b  ?
    (DELAY
     (FV (b)
	 (cond ((pair? b)
		(bag-fold-right (car b)
				(bag-fold-right (cdr b) start fn)
				fn))
	       ((null? b)
		start)
	       (else
		(fn b start)))))))

(def (bag->list b #!optional (tail '()))
     (bag-fold-right b tail cons))

(def (bag->stream b #!optional (tail '()))
     (bag-stream-fold-right b tail cons))


(def (bag->string b #!optional (display display))
     (with-output-to-string (C bag-for-each b display)))

(TEST
 > (def bags '(()
	       a
	       (a)
	       (a b)
	       (a . b)
	       (a (b . c))
	       (a (b) c)
	       (a (b) . c)
	       (a (b . c) . d)))

 > (map (bag-of symbol?) bags)
 (#t #t #t #t #t #t #t #t #t)
 > (map (bag-of string?) bags)
 (#t #f #f #f #f #f #f #f #f)
 > (map (bag-of any?) bags)
 (#t #t #t #t #t #t #t #t #t)

 > (def ss (map bag->list bags))
 > ss
 (()
  (a)
  (a)
  (a b)
  (a b)
  (a b c)
  (a b c)
  (a b c)
  (a b c d))

 > (equal? ss (map (comp-function reverse bag->reverse-list) bags))
 #t

 > (equal? ss (map (comp-function stream->list bag->stream) bags))
 #t

 > (map (comp-function promise? bag->stream) bags)
 (#t #t #t #t #t #t #t #t #t)
 
 > (map bag-length bags)
 (0 ;;()
  1 ;;(a)
  1 ;;(a)
  2 ;;(a b)
  2 ;;(a b)
  3 ;;(a b c)
  3 ;;(a b c)
  3 ;;(a b c)
  4 ;;(a b c d)
  )

 > (equal? # (map (C bag-fold _ 0 (lambda (v tot)
				    (+ tot 1)))
		  bags))
 #t

 > (map (lambda_ (with-output-to-string (& (bag-for-each _ print))))
	bags)
 (""
  "a"
  "a"
  "ab"
  "ab"
  "abc"
  "abc"
  "abc"
  "abcd"))
