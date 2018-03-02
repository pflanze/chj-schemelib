;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 (cj-env-1 dec inc)
	 cj-symbol
	 (list-util let-pair)
	 (srfi-11 apply-values)
	 (lazy FV))

(export right-associate
	left-associate
	syntax:right-associate
	syntax:left-associate
	(macro RA)
	(macro LA)
	compose**
	(macro compose*)
	(macro compose-1ary)
	(macro compose/arity)
	true/0
	true/1
	false/0
	false/1
	just?
	maybe
	on* ;; for binary |on| see cj-env
	(macro or**)
	(macro or/)
	<to<=
	sorted-list-of)




;; n-ary:

(define (right-associate fn lis error)
  (if (null? lis)
      (error "got no element")
      (if (null? (cdr lis))
	  (car lis)
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
 (c c)
 > (map (lambda (_)
	  (with-exception-catcher error-exception-message
				  (lambda () (_ 'comp (list) error))))
	(list syntax:right-associate syntax:left-associate))
 ("got no element" "got no element")
 )

(define-macro* (RA op . exprs)
  (syntax:right-associate op exprs
			  (lambda (msg)
			    (source-error stx msg))))

(define-macro* (LA op . exprs)
  (syntax:left-associate op exprs
			 (lambda (msg)
			   (source-error stx msg))))

(TEST
 > (expansion#RA compose-function half x*y inc2values)
 (compose-function half (compose-function x*y inc2values))
 > (expansion#LA compose-function half x*y inc2values)
 (compose-function (compose-function half x*y) inc2values)
 )

;; as a function:

(define (compose** . fs)
  (right-associate compose-function fs error))

;; as macro for a tad more performance:

(IF #t
    (define-macro* (compose* . f-exprs)
      `(RA compose-function ,@f-exprs))
    ;; or, manually inlining the compose-function rule:
    ;; -- why using apply-values here??
    (define-macro* (compose* . f-exprs)
      (define X (gensym 'x))
      `(lambda ,X
	 ,(fold-right (lambda (f-expr inner)
			`(apply-values ,f-expr ,inner))
		      `(apply values ,X)
		      f-exprs))))

;; 1-ary version for more performance (and just as macro for same
;; reason, OK?):
(define-macro* (compose-1ary . es)
  (with-gensym X
	       `(lambda (,X)
		  ,(fold-right (lambda (e inner)
				 `(,e ,inner))
			       X
			       es))))

;; with parametrizable arity:
(define-macro* (compose/arity n . es)
  (assert* natural0? n
	   (lambda (n)
	     (let* ((ARGS (map (lambda (n) (gensym)) (iota n)))
		    (code (fold-right (lambda (e inner)
					`((,e ,@inner)))
				      ARGS
				      es)))
	       (if (and (pair? code)
			(null? (cdr code)))
		   `(lambda ,ARGS
		      ,(car code))
		   (error "bug"))))))


(TEST
 > (define TEST:equal? syntax-equal?)

 > (expansion#compose-1ary a b c)
 (lambda (GEN:X-3566) (a (b (c GEN:X-3566))))
 > (expansion#compose-1ary a)
 (lambda (GEN:X-3567) (a GEN:X-3567))

 > (expansion#compose/arity 1 a b c)
 (lambda (GEN:-3382) (a (b (c GEN:-3382))))
 > (expansion#compose/arity 3 a b c)
 (lambda (GEN:-3383 GEN:-3384 GEN:-3385)
   (a (b (c GEN:-3383 GEN:-3384 GEN:-3385)))))



(TEST
 > (define (half x) (/ x 2))
 > (define (square x) (* x x))
 > (define x*y (lambda-values ((x y)) (* x y)))
 > ((compose** half inc square) 10)
 101/2
 > ((compose-1ary half inc square) 10)
 101/2
 > ((compose/arity 1 half inc square) 10)
 101/2

 > ((compose** half inc x*y) (values 10 20))
 201/2
 > ((compose-1ary half inc x*y) (values 10 20))
 201/2
 > ((compose/arity 1 half inc x*y) (values 10 20))
 201/2

 > (define (inc2values x y) (values (inc x) (inc y)))
 > ((compose** half x*y inc2values) 10 20)
 231/2
 > ((compose* half x*y inc2values) 10 20)
 231/2
 > ((compose/arity 2 half x*y inc2values) 10 20)
 231/2

 ;; compose-function is ("fully") associative (left or right doesn't matter),
 ;; so choosing right-associate was arbitrary
 > ((compose-function (compose-function half x*y) inc2values) 10 20)
 231/2
 > ((compose-function half (compose-function x*y inc2values)) 10 20)
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

(define-macro* (or** true? . clauses)
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
      (or** (lambda (v)
	      (not (eq? v ,FALSE-VALUE)))
	    ,@clauses))))

(TEST
 > (or/ 'false 'false 'nonfalse 'anothernonfalse)
 nonfalse
 > (or/ 'false 'false 'false)
 #f
 )


(define <to<=
  ;; <-><= would be a fun name, wouldn't it?
  (compose-function complement flip))

(define (sorted-list-of el? <)
  (strictly-monotonic-list-of el? (<to<= <)))

(TEST
 > ((sorted-list-of number? <) '(1 2 3))
 #t
 > ((sorted-list-of number? <) '(3 2 1))
 #f
 > ((sorted-list-of number? >) '(3 2 1))
 #t
 > ((sorted-list-of number? >) '(3 2 2))
 #t
 > ((sorted-list-of number? >=) '(3 2 2))
 #f ;; hehe kinda nonsensical now ok?
 > ((sorted-list-of number? <) '(1 2 2))
 #t
 > ((sorted-list-of number? <=) '(1 2 2))
 #f ;; dito
 > ((sorted-list-of number? >) '())
 #t)


