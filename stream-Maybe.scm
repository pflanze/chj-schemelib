;;; Copyright 2016-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 Maybe
	 (list-util let-pair)
	 (lazy FV)
	 (cj-env-2 C)
	 define-strict-and-lazy
	 test)

(export mapfilter/tail stream-mapfilter/tail
	mapfilter stream-mapfilter
	list-Maybe-ref stream-Maybe-ref
	)


;; Also see stream-filter-map etc. in stream.scm !

;; only 1-ary for now
(define-strict-and-lazy
  mapfilter/tail
  stream-mapfilter/tail
  (named self
	 (lambda (fn tail l)
	   (DELAY
	    (FV (l)
		(if (null? l)
		    tail
		    (let-pair ((a l*) l)
			      (let ((rest (& (self fn tail l*))))
				(Maybe:cond ((fn a) =>
					     (lambda (v)
					       (cons v
						     (rest))))
					    (else
					     (rest)))))))))))


;; only 1-ary for now
(def (stream-mapfilter fn l)
     (stream-mapfilter/tail fn '() l))

(def (mapfilter fn l)
     (mapfilter/tail fn '() l))


(TEST
 > (def s (stream-mapfilter
	   (lambda (v) (if (even? v) (Just v) (Nothing))) '(1 3 4 -2 -1)))
 > (promise? s)
 #t
 > (stream->list s)
 (4 -2)
 > (mapfilter (lambda (v) (if (even? v) (Just v) (Nothing))) '(1 3 4 -2 -1))
 (4 -2))


(define-strict-and-lazy
  list-Maybe-ref
  stream-Maybe-ref
  (named lp
	 (lambda (l i)
	   (FV (l)
	       (if (pair? l)
		   (if (fx> i 0)
		       (lp (cdr l)
			   (fx- i 1))
		       (if (fxzero? i)
			   (Just (car l))
			   (error "list-Maybe-ref: index not natural0:" i)))
		   (if (null? l)
		       (Nothing)
		       (error "list-Maybe-ref: improper list or stream:" l)))))))

(TEST
 > (stream-Maybe-ref (stream-iota) 0)
 #((Just) 0)
 > (stream-Maybe-ref (stream-iota) 10)
 #((Just) 10)
 > (stream-Maybe-ref (stream-iota 8) 10)
 #((Nothing))
 > (%try-error (stream-Maybe-ref (stream-iota) -1))
 #(error "list-Maybe-ref: index not natural0:" -1)
 > (%try (stream-Maybe-ref (stream-iota) 1.0))
 (exception text: "(Argument 1) FIXNUM expected\n(fx> 1. 0)\n")
 > (stream-Maybe-ref (cons 4 5) 0)
 #((Just) 4)
 > (%try (stream-Maybe-ref (cons 4 5) 1))
 (exception text: "list-Maybe-ref: improper list or stream: 5\n"))

