;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.test)
	 (lib.cj-env)
	 (lib.cj-symbol)
	 (lib.list-util) ;; let-pair
	 )


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
(define (either . fs)
  (if (null? fs)
      (lambda x
	#f)
      (let-pair ((f fs*) fs)
		((lambda (r)
		   (lambda x
		     (or (apply f x)
			 (apply r x))))
		 (apply either fs*)))))
(TEST
 > ((either symbol? string?) "foo")
 #t
 > ((either symbol? string?) 'bar)
 #t
 > ((either symbol? string?) 0)
 #f
 > ((either symbol? number? string?) 0)
 #t
 ;; test shortcutting?
 )

;; name ok?
(define (both f0 f1)
  (lambda x
    (and (apply f0 x)
	 (apply f1 x))))

(TEST
 > ((both even? odd?) 1)
 #f
 > ((both even? odd?) 2)
 #f
 > ((both even? negative?) 2)
 #f
 > ((both even? negative?) -2)
 #t
 > ((both even? negative?) -1)
 #f
 )

;; name?
(define (all-of . preds)
  (lambda x
    (every (lambda (pred)
	     (apply pred x))
	   preds)))

(TEST
 > ((all-of even? odd?) 1)
 #f
 > ((all-of even? odd?) 2)
 #f
 > ((all-of even? negative?) 2)
 #f
 > ((all-of even? negative?) -2)
 #t
 > ((all-of even? negative?) -1)
 #f

 > ((all-of odd?) 1)
 #t
 > ((all-of) 1)
 #t
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
		  (fn (car lis)
		      (car lis*))
		  (fn (car lis)
		      (rec lis*))))))))

(define (left-associate fn lis error)
  (right-associate (lambda (a b)
		     (fn b a))
		   (reverse lis)
		   error))

(define (syntax:_-associate _-associate)
  (lambda (op lis error)
    (_-associate (lambda (a b)
		   (list op a b))
		 lis
		 error)))

(define syntax:right-associate (syntax:_-associate right-associate))
(define syntax:left-associate (syntax:_-associate left-associate))

(TEST
 > (syntax:right-associate 'comp (list 'a 'b 'c) error)
 (comp a (comp b c))
 > (syntax:right-associate 'comp (list 'b 'c) error)
 (comp b c)

 ;; and left...
 > (syntax:left-associate 'comp (list 'a 'b 'c) error)
 (comp (comp a b) c)
 > (syntax:left-associate 'comp (list 'b 'c) error)
 (comp b c)

 ;; and errors:
 > (map (lambda (_)
	  (with-exception-catcher error-exception-message
				  (lambda () (_ 'comp (list 'c) error))))
	(list syntax:right-associate syntax:left-associate))
 ("got only one element, need two" "got only one element, need two")
 > (map (lambda (_)
	  (with-exception-catcher error-exception-message
				  (lambda () (_ 'comp (list) error))))
	(list syntax:right-associate syntax:left-associate))
 ("got no element, need two" "got no element, need two")
 )

(define-macro* (Ra op . exprs)
  (syntax:right-associate op exprs
			  (lambda (msg)
			    (source-error stx msg))))

(define-macro* (La op . exprs)
  (syntax:left-associate op exprs
			 (lambda (msg)
			   (source-error stx msg))))

(TEST
 > (expansion#Ra compose half x*y inc2values)
 (compose half (compose x*y inc2values))
 > (expansion#La compose half x*y inc2values)
 (compose (compose half x*y) inc2values)
 )

;; as a function:

(define (compose** . fs)
  (right-associate compose fs error))

;; as macro for a tad more performance:

(IF #t
    (define-macro* (compose* . f-exprs)
      `(Ra compose ,@f-exprs))
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


;; move to lib pure booleans?
(define (just? v)
  (and v #t))

;; and to a lib for maybe handling?
(define (_-maybe fn)
  (lambda (v)
    (and v
	 (fn v))))

(define (maybe pred)
  (either not pred))

(TEST
 > ((maybe number?) 1)
 #t
 > ((maybe number?) #f)
 #t
 > ((maybe number?) #t)
 #f
 )


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

(define (list-of pred)
  (lambda (x)
    (and (list? x)
	 (every pred x))))


