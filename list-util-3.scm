;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.



(require (list-util let-pair)
	 named
	 test
	 local-test
	 (srfi-11 letv values->vector lambda-values)
	 (cj-functional compose*))

(export	list-starts-with?/equal? list-starts-with?
	char-list-starts-with-string?
	)



;; like string-starts-with?, but returns the remainder, too? (And,
;; taking a comparison predicate.) All the beginnings of a parser
;; combinator library, really...
;; Q: how does Haskell keep uniformity when going vector representation?

(define (list-starts-with?/equal? equal?)
  (named starts-with?
	 (lambda (l matchl)
	   (if (null? l)
	       (values (null? matchl)
		       l)
	       (if (null? matchl)
		   (values #t
			   l)
		   (let-pair ((m matchl*) matchl)
			     (let-pair ((a l*) l)
				       (if (equal? m a) ;; reverse arg order?
					   (starts-with? l* matchl*)
					   (values #f
						   l)))))))))

(define list-starts-with? (list-starts-with?/equal? equal?))

(TEST
 > (def (lut t)
	(local-TEST
	 > (t "abcd" "ab")
	 #(#t "cd")
	 > (t "abcd" "abx")
	 #(#f "cd")
	 > (t "abcd" "")
	 #(#t "abcd")
	 > (t "ab" "abx")
	 #(#f "")
	 > (t "ab" "ab")
	 #(#t "")
	 > (t "ab" "ax")
	 #(#f "b")))
 > (lut (compose* (lambda-values ((b l))
			    (vector b (list->string l)))
		  (applying list-starts-with?)
		  (lambda args (map string->list args)))))


(define (char-list-starts-with-string? l str)
  (let ((len (string-length str)))
    (let lp ((l l)
	     (i 0))
      (if (fx< i len)
	  (if (null? l)
	      (values #f l)
	      (let-pair ((a l*) l)
			(if (char=? a (string-ref str i))
			    (lp l* (fx+ i 1))
			    (values #f l))))
	  (values #t l)))))

(TEST
 > (lut (compose* (lambda-values ((b l))
			    (vector b (list->string l)))
		  (lambda (input match)
		    (char-list-starts-with-string? (string->list input) match)))))

