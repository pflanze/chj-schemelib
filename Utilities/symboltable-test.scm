;;; Copyright 2010 by Christian Jaeger <chrjae@gmail.com>

;;; This file is part of GIT System.
;;;
;;;    GIT System is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU Lesser General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    GIT System is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU Lesser General Public License for more details.
;;;
;;;    You should have received a copy of the GNU Lesser General Public License
;;;    along with GIT System.  If not, see <http://www.gnu.org/licenses/>.

;;;
;;;; Randomized tests and benchmarking
;;;

;; just to stay 'compatible' to persistentidentifier:
(define (make-symbol id maybe-parent name)
  (values (inc id)
	  (string->symbol
	   (string-append (if maybe-parent (string-append maybe-parent ":")
			      "")
			  name
			  "."
			  (number->string id)))))

(TEST
 ;; randomized-test data
 > (define (strings) (pseudorandomsource*->a-z-string-stream
		      (make-pseudorandomsource 10 11)
		      (make-pseudorandomsource 10 13)
		      (make-range 3 6)))
 > (define (lengths) (pseudorandomsource->integer-stream
		      (make-pseudorandomsource 103 114)
		      (make-range 0 600)))
 > (define (gaplengths #!optional (prs (make-pseudorandomsource 114 103)))
     (stream-map (lambda (n)
		   (expt (* n 1/100 1.5) 10))
		 (pseudorandomsource->integer-stream
		  prs
		  (make-range 0 100))))
 
 > (define (mkpis from-id)
     (define maybe-parent #f)
     ;; (let rec ((id from-id)
     ;; 	       (strs (strings)))
     ;;   (delay
     ;; 	 (let-pair ((str strs*) (force strs))
     ;; 		   (letv ((id* pi) (make-symbol id maybe-parent str))
     ;; 			 (cons pi
     ;; 			       (rec id* strs*))))))

     ;; or: 
     (stream-unfold2
      (lambda (x) #f) ;; or (lambda-values ((id strs)) (null? (force strs)))
      (lambda-values
       ((id strs))
       (let-pair ((str strs*) (force strs))
		 (letv ((id* pi) (make-symbol id maybe-parent str))
		       (values pi
			       (values id* strs*)))))
      (values from-id
	      (strings))))
 > (F (stream-take (mkpis 1000) 10))
 (
  jsipz.1000
  kqw.1001
  qhro.1002
  cfupo.1003
  kzv.1004
  irqb.1005
  ica.1006
  ctpm.1007
  ivy.1008
  ntj.1009
  )

 > (define (_split-gaps gaplens pis) ;; -> (values gaps nongaps)
     ;; (back to open coding. already for filter-gaps.)
     (no-delay
      (if (null? (force gaplens))
	  (values 'A 'A)
	  (let-pair ((gaplen gaplens*) (force gaplens))
		    (let next-in-gap
			((gaplen gaplen)
			 (pis pis))
		      (if (null? (force pis))
			  (values 'B 'B)
			  (let-pair ((pi pis*) (force pis))
				    (if (< gaplen 1)
					;; nongap output
					(letv ((gaps nongaps) (_split-gaps gaplens* pis*))
					      (values gaps
						      (cons pi nongaps)))

					(letv ((gaps nongaps) (next-in-gap (- gaplen 1)
									   pis*))
					      (values (cons pi gaps)
						      nongaps))))))))))
 ;; ^ this is worthless because streaming doesn't work. Have to walk the input independently.
 > (define filter-nongaps
     (named-lambda
	 nextI (gaplens pis)
	 ;; back to open coding. GR
	 (delay
	   (let-pair ((gaplen gaplens*) (force gaplens))
		     (let nextII ((gaplen gaplen)
				  (pis pis))
		       (let-pair ((pi pis*) (force pis))
				 (if (< gaplen 1)
				     (cons pi
					   (nextI gaplens* pis*))
				     (nextII (- gaplen 1)
					     pis*))))))))
 > (define filter-gaps
     (named-lambda
	 nextI (gaplens pis)
	 ;; back to open coding. GR
	 (delay
	   (let-pair ((gaplen gaplens*) (force gaplens))
		     (let nextII ((gaplen gaplen)
				  (pis pis))
		       (let-pair ((pi pis*) (force pis))
				 (if (< gaplen 1)
				     ;; the difference from filter-nongaps :
				     (nextI gaplens* pis*)
				     (cons pi
					   (nextII (- gaplen 1)
						   pis*)))))))))
 > (define (split-gaps gaplens pis) ;; -> (values nongaps gaps)
     (values (filter-nongaps gaplens pis)
	     (filter-gaps gaplens pis)))

 > (define-values (nongaps gaps) (split-gaps (gaplengths) (mkpis 1000)))
 > (F (stream-take nongaps 8))
 (
  kqw.1001
  kzv.1004
  irqb.1005
  ica.1006
  ctpm.1007
  ntj.1009
  btbb.1010
  tqpvi.1011
  )
 > (F (stream-take gaps 8)) ;; which are more spaced than nongaps
 (
  jsipz.1000
  qhro.1002
  cfupo.1003
  ivy.1008
  deq.1023
  zuka.1024
  nft.1025
  klnpp.1026
  )

 > (define (mkmapping from-id)
     (values (stream-map cons
			 (filter-nongaps (gaplengths)
					 (mkpis from-id))
			 (stream-iota))
	     ;; thanks to taking new streams below, there is no
	     ;; holding on to memory
	     (filter-gaps (gaplengths) 
			  (mkpis from-id))))
 > (map floor (F (stream-take (gaplengths) 25)))
 (1. 2. 0. 0. 0. 1. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 4. 0. 0. 0. 0. 17.)
 > (define-values (nongaps gaps) (mkmapping 1000))
 > (F (stream-take nongaps 10))
 (
  (kqw.1001 . 0)
  (kzv.1004 . 1)
  (irqb.1005 . 2)
  (ica.1006 . 3)
  (ctpm.1007 . 4)
  (ntj.1009 . 5)
  (btbb.1010 . 6)
  (tqpvi.1011 . 7)
  (wutn.1012 . 8)
  (hgmh.1013 . 9)
  )
 > (drop (F (stream-take nongaps 30)) 20)
 (
  (ezqvs.1028 . 20)
  (gngb.1029 . 21)
  (rornt.1030 . 22)
  (xeukj.1031 . 23)
  (hile.1049 . 24)
  (bzdoi.1050 . 25)
  (ssawn.1051 . 26)
  (bog.1052 . 27)
  (gvm.1053 . 28)
  (tuxww.1054 . 29)
  )

 ;; the actual symboltable tests:

 > (define (mkmapping+symboltable id len) ;; -> (values m t gaps)
     (letv ((nongaps gaps) (mkmapping id))
	   (let ((m (F (stream-take nongaps len))))
	     (values m
		     (list->symboltable m)
		     (F (stream-take gaps len))))))
 ;;> (define-values (m t gaps) (mkmapping+symboltable 1000 30))
 > (define (sum l) (fold + 0 l))
 > (define test-mapping+symboltable
     (lambda-values ((m t gaps))
		    (let ((sum1 (sum (map cdr m)))
			  (sum2 (sum (map (lambda (pi)
					    (symboltable-ref t pi 'not-found))
					  (map car m))))
			  (invalid-gaps (filter (lambda (v)
						  (not (eq? v 'not-found2)))
						(map (lambda (pi)
						       (symboltable-ref t pi 'not-found2))
						     gaps))))
		      (values sum1 sum2 invalid-gaps))))
 > (values->vector (test-mapping+symboltable (mkmapping+symboltable 1000 10)))
 #(45 45 ())
 > (values->vector (test-mapping+symboltable (mkmapping+symboltable 1000 1038)))
 #(538203 538203 ())
 > (values->vector (test-mapping+symboltable (mkmapping+symboltable 10 1038)))
 #(538203 538203 ())

 > (define (test-for len)
     (letv ((sum1 sum2 invalid-gaps) (test-mapping+symboltable (mkmapping+symboltable 1000 len)))
	   (and (= sum1 sum2)
		(null? invalid-gaps))))
 
 > (map test-for (F (stream-take (lengths) 20)))
 (#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)

 ;; try with a skewed distribution, too (yeah hacky):
 > (define (floor-integer n)
     (inexact->exact (floor n)))
 > (map test-for (F (stream-take (stream-map (compose floor-integer *)
 					     (lengths)
 					     (gaplengths (make-pseudorandomsource 773 31)))
 				 20)))
 (#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)


 ;; test -add
 > (define testsize 2000)
 > (define t (stream-fold-left (lambda (k.v t) (symboltable-add t (car k.v) (cdr k.v))) empty-symboltable (stream-take nongaps testsize)))
 > (= (symboltable-length t) testsize)
 #t
 > (stream-fold-left (lambda (k.v res)
		       (and res (= (symboltable-ref t (car k.v) 'not-found)
				   (cdr k.v))))
		     #t
		     (stream-take nongaps testsize))
 #t
 > (stream-fold-left (lambda (k res)
		       (and res (eq? (symboltable-ref t k 'not-found)
				     'not-found)))
		     #t
		     (stream-take gaps testsize))
 ;; ^ here the number of elements doesn't actually matter
 #t


 ;; test symboltable->list
 > (define (test-symboltable->list n)
     (letv ((m t gaps) (mkmapping+symboltable 100 n))
	   (equal? (sort (symboltable->list t) (on cdr <))
		   m)))
 > (fold andf #t (map test-symboltable->list (iota 50)))
 #t
 
 )

(define (bench-symbol len n)
  (declare (standard-bindings)
	   (extended-bindings)
	   (fixnum)
	   (not safe))
  (letv ((m t gaps) (mkmapping+symboltable 1000 len))
	(let ((nongaps (map car m)))
	  (let ((b (lambda (l)
		     (let lp1 ((n n)
			       (v '?1))
		       (if (positive? n)
			   (let lp2 ((l l)
				     (v '?2))
			     (if (null? l)
				 (lp1 (dec n)
				      v)
				 (lp2 (cdr l)
				      (symboltable-ref t (car l) 'not-found3))))
			   v))))
		(pp (lambda (v)
		      (##pretty-print v (current-output-port)))))
	    (pp (time (b nongaps)))
	    (pp (time (b gaps)))))))
