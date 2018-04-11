;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.



(require (list-util let-pair)
	 named
	 test
	 local-test
	 (srfi-11 letv values->vector lambda-values)
	 (cj-functional compose*)
	 lazy
	 debuggable-promise
	 cj-match
	 )

;; XX rename this to iseq-util-3 or something, change prefixes, or
;; what? All functions here also accept streams even though still
;; naming them *list*. -- except move out let-list
(export	list-starts-with?/equal? list-starts-with?
	char-list-starts-with-string?
	(macro let-list))

(possibly-use-debuggable-promise)

;; like string-starts-with?, but returns the remainder, too? (And,
;; taking a comparison predicate.) All the beginnings of a parser
;; combinator library, really...
;; Q: how does Haskell keep uniformity when going vector representation?

(define (list-starts-with?/equal? equal?)
  (named
   starts-with?
   (lambda (l matchl)
     (FV (l)
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
						 l))))))))))

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
      (FV (l)
	  (if (fx< i len)
	      (if (null? l)
		  (values #f l)
		  (let-pair ((a l*) l)
			    (if (char=? a (string-ref str i))
				(lp l* (fx+ i 1))
				(values #f l))))
	      (values #t l))))))

(TEST
 > (lut (compose*
	 (lambda-values ((b l))
			(vector b (list->string l)))
	 (lambda (input match)
	   (char-list-starts-with-string? (string->list input) match)))))


;; We were tempted to call this |letl| at first, like |letv|, since it
;; only allows to decompose one value after all. But, that's quite a
;; moot argument as all the other decomposition forms like let-pair or
;; let-$anystructure only take one, too. And we forgot that we named
;; it letl.
(define-macro* (let-list bind . body)
  (mcase bind
	 (`(`vars `expr)
	  (with-gensyms
	   (V ERR)
	   `(let* ((,V ,expr)
		   (,ERR (lambda ()
			   (error ,(string-append
				    "let-list: expected a list containing "
				    (object->string (cj-desourcify vars))
				    " but got:")
				  ,V))))
	      ,(let rec ((vars (source-code vars)))
		 (cond ((null? vars)
			`(if (null? ,V)
			     (let ()
			       ,@body)
			     (,ERR)))
		       ((pair? vars)
			(let-pair ((var vars*) vars)
				  `(if (pair? ,V)
				       (let ((,var (car ,V))
					     (,V (cdr ,V)))
					 ,(rec vars*))
				       (,ERR))))
		       (else
			;; rest argument
			`(let ((,vars ,V))
			   ,@body)))))))))

(TEST
 > (%try-error (let-list ((a b d e f) (list 10 22 33)) b))
 [error "let-list: expected a list containing (a b d e f) but got:" (10 22 33)]
 > (let-list ((a b d) (list 10 22 33)) b)
 22
 > (%try-error (let-list ((a b) (list 10 22 33)) b))
 [error "let-list: expected a list containing (a b) but got:" (10 22 33)]
 > (let-list (() (list)) "foo")
 "foo"
 ;; rest arguments
 > (let-list ((a e . i) (list 10 22 33)) (vector a e i))
 [10 22 (33)]
 > (let-list ((a e . i) (list 10 22)) (vector a e i))
 [10 22 ()]
 > (%try-error (let-list ((a e . i) (list 10)) (vector a e i)))
 [error "let-list: expected a list containing (a e . i) but got:" (10)])

