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
     ;; 		   (letv ((id* pi) (make-persistentidentifier id maybe-parent str))
     ;; 			 (cons pi
     ;; 			       (rec id* strs*))))))

     ;; or: 
     (stream-unfold2
      (lambda (x) #f) ;; or (lambda-values ((id strs)) (null? (force strs)))
      (lambda-values
       ((id strs))
       (let-pair ((str strs*) (force strs))
		 (letv ((id* pi) (make-persistentidentifier id maybe-parent str))
		       (values pi
			       (values id* strs*)))))
      (values from-id
	      (strings))))
 > (F (stream-take (mkpis 1000) 10))
 (#(persistentidentifier 1000 #f "jsipz")
   #(persistentidentifier 1001 #f "kqw")
   #(persistentidentifier 1002 #f "qhro")
   #(persistentidentifier 1003 #f "cfupo")
   #(persistentidentifier 1004 #f "kzv")
   #(persistentidentifier 1005 #f "irqb")
   #(persistentidentifier 1006 #f "ica")
   #(persistentidentifier 1007 #f "ctpm")
   #(persistentidentifier 1008 #f "ivy")
   #(persistentidentifier 1009 #f "ntj"))

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
 (#(persistentidentifier 1001 #f "kqw")
   #(persistentidentifier 1004 #f "kzv")
   #(persistentidentifier 1005 #f "irqb")
   #(persistentidentifier 1006 #f "ica")
   #(persistentidentifier 1007 #f "ctpm")
   #(persistentidentifier 1009 #f "ntj")
   #(persistentidentifier 1010 #f "btbb")
   #(persistentidentifier 1011 #f "tqpvi"))
 > (F (stream-take gaps 8)) ;; which are more spaced than nongaps
 (#(persistentidentifier 1000 #f "jsipz")
   #(persistentidentifier 1002 #f "qhro")
   #(persistentidentifier 1003 #f "cfupo")
   #(persistentidentifier 1008 #f "ivy")
   #(persistentidentifier 1023 #f "deq")
   #(persistentidentifier 1024 #f "zuka")
   #(persistentidentifier 1025 #f "nft")
   #(persistentidentifier 1026 #f "klnpp"))

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
 ((#(persistentidentifier 1001 #f "kqw") . 0)
  (#(persistentidentifier 1004 #f "kzv") . 1)
  (#(persistentidentifier 1005 #f "irqb") . 2)
  (#(persistentidentifier 1006 #f "ica") . 3)
  (#(persistentidentifier 1007 #f "ctpm") . 4)
  (#(persistentidentifier 1009 #f "ntj") . 5)
  (#(persistentidentifier 1010 #f "btbb") . 6)
  (#(persistentidentifier 1011 #f "tqpvi") . 7)
  (#(persistentidentifier 1012 #f "wutn") . 8)
  (#(persistentidentifier 1013 #f "hgmh") . 9))
 > (drop (F (stream-take nongaps 30)) 20)
 ((#(persistentidentifier 1028 #f "ezqvs") . 20)
  (#(persistentidentifier 1029 #f "gngb") . 21)
  (#(persistentidentifier 1030 #f "rornt") . 22)
  (#(persistentidentifier 1031 #f "xeukj") . 23)
  (#(persistentidentifier 1049 #f "hile") . 24)
  (#(persistentidentifier 1050 #f "bzdoi") . 25)
  (#(persistentidentifier 1051 #f "ssawn") . 26)
  (#(persistentidentifier 1052 #f "bog") . 27)
  (#(persistentidentifier 1053 #f "gvm") . 28)
  (#(persistentidentifier 1054 #f "tuxww") . 29))

 ;; the actual pitable tests:

 > (define (mkmapping+pitable id len) ;; -> (values m t gaps)
     (letv ((nongaps gaps) (mkmapping id))
	   (let ((m (F (stream-take nongaps len))))
	     (values m
		     (list->pitable m)
		     (F (stream-take gaps len))))))
 ;;> (define-values (m t gaps) (mkmapping+pitable 1000 30))
 > (define (sum l) (fold + 0 l))
 > (define test-mapping+pitable
     (lambda-values ((m t gaps))
		    (let ((sum1 (sum (map cdr m)))
			  (sum2 (sum (map (lambda (pi)
					    (pitable-ref t pi 'not-found))
					  (map car m))))
			  (invalid-gaps (filter (lambda (v)
						  (not (eq? v 'not-found2)))
						(map (lambda (pi)
						       (pitable-ref t pi 'not-found2))
						     gaps))))
		      (values sum1 sum2 invalid-gaps))))
 > (values->vector (test-mapping+pitable (mkmapping+pitable 1000 10)))
 #(45 45 ())
 > (values->vector (test-mapping+pitable (mkmapping+pitable 1000 1038)))
 #(538203 538203 ())
 > (values->vector (test-mapping+pitable (mkmapping+pitable 10 1038)))
 #(538203 538203 ())

 > (define (test-for len)
     (letv ((sum1 sum2 invalid-gaps) (test-mapping+pitable (mkmapping+pitable 1000 len)))
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

 )

(define (bench-pi len n)
  (declare (standard-bindings)
	   (extended-bindings)
	   (fixnum)
	   (not safe))
  (letv ((m t gaps) (mkmapping+pitable 1000 len))
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
				      (pitable-ref t (car l) 'not-found3))))
			   v))))
		(pp (lambda (v)
		      (##pretty-print v (current-output-port)))))
	    (pp (time (b nongaps)))
	    (pp (time (b gaps)))))))
