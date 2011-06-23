;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


(define (flip f)
  (lambda (x y)
    (f y x)))

(define (complement fn)
  (lambda v
    (not (apply fn v))))

(define (compose f g)
  (lambda x
    (f (apply g x))))

;; name?
(define (or-apply f g)
  (lambda x
    (or (apply f x)
	(apply g x))))
(TEST
 > ((or-apply symbol? string?) "foo")
 #t
 > ((or-apply symbol? string?) 'bar)
 #t
 > ((or-apply symbol? string?) 0)
 #f
 )

;; n-ary:

(define (right-associate fn lis error)
  (if (null? lis)
      (error "got no element, need two")
      (if (null? (cdr lis))
	  (error "got only one element, need two")
	  (let rec ((lis lis))
	    (let* ((lis* (cdr lis))
		   (lis** (cdr lis*)))
	      (if (null? lis**)
		  (fn
			(car lis)
			(car lis*))
		  (fn
			(car lis)
			(rec lis*))))))))

(define (syntax:right-associate op lis error)
  (right-associate (lambda (a b)
		     (list op a b))
		   lis
		   error))

(TEST
 > (syntax:right-associate 'comp (list 'a 'b 'c) error)
 (comp a (comp b c))
 > (syntax:right-associate 'comp (list 'b 'c) error)
 (comp b c)
 > (with-exception-catcher error-exception-message (lambda () (syntax:right-associate 'comp (list 'c) error)))
 "got only one element, need two"
 > (with-exception-catcher error-exception-message (lambda () (syntax:right-associate 'comp (list) error)))
 "got no element, need two"
 )

(define-macro* (R op . exprs)
  (syntax:right-associate op exprs
			  (lambda (msg)
			    (source-error stx msg))))

(TEST
 > (expansion#R compose half x*y inc2values)
 (compose half (compose x*y inc2values))
 )

;; as a function:

(define (compose** . fs)
  (right-associate compose fs error))

;; as macro for a tad more performance:

(IF #t
    (define-macro* (compose* . f-exprs)
      `(R compose ,@f-exprs))
    ;; or, manually inlining the compose rule:
    (define-macro* (compose* . f-exprs)
      (define X (gensym 'x))
      `(lambda ,X
	 ,(fold-right (lambda (f-expr inner)
			`(apply-values ,f-expr ,inner))
		      `(apply values ,X)
		      f-exprs))))


(TEST
 > (define (half x) (/ x 2))
 > (define (square x) (* x x))
 > (define x*y (lambda-values ((x y)) (* x y)))
 > ((compose** half inc square) 10)
 101/2
 > ((compose** half inc x*y) (values 10 20))
 201/2
 > (define (inc2values x y) (values (inc x) (inc y)))
 > ((compose** half x*y inc2values) 10 20)
 231/2
 > ((compose* half x*y inc2values) 10 20)
 231/2
 ;; compose is ("fully") associative (left or right doesn't matter),
 ;; so choosing right-associate was arbitrary
 > ((compose (compose half x*y) inc2values) 10 20)
 231/2
 > ((compose half (compose x*y inc2values)) 10 20)
 231/2

 )


(define (true/0)
  #t)
(define (true/1 x)
  #t)

(define (false/0)
  #f)
(define (false/1 x)
  #f)


;; n-ary "on"
;; for binary |on| see cj-env

(define (on* fn1 fn2)
  (lambda args
    (apply fn2 (map fn1 args))))


;; an "or" that triggers on something else than #f (but returns #f on
;; 'failure'):

(define-macro* (or* true? . clauses)
  (with-gensyms
   (TRUE? V)
   `(let ((,TRUE? ,true?))
      ,(let rec ((clauses clauses))
	 (if (null? clauses)
	     `#f
	     `(let ((,V ,(car clauses)))
		(if (,TRUE? ,V)
		    ,V
		    ,(rec (cdr clauses)))))))))

;; and build a true? predicate easily:

(define-macro* (or/ false-value . clauses)
  (with-gensym
   FALSE-VALUE
   `(let ((,FALSE-VALUE ,false-value))
      (or* (lambda (v)
	     (not (eq? v ,FALSE-VALUE)))
	   ,@clauses))))

(TEST
 > (or/ 'false 'false 'nonfalse 'anothernonfalse)
 nonfalse
 > (or/ 'false 'false 'false)
 #f
 )
