;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 (fixnum inc dec)
	 cj-symbol
	 (list-util let-pair)
	 (srfi-11 apply-values)
	 (lazy FV)
	 (cj-gambit-sys maybe-decompile)
	 ;;(cj-source-util schemedefinition-arity:pattern->template)
	 (cj-functional-2 compose compose-function either)
	 (code-util early-bind-expressions))

(export right-associate
	left-associate
	syntax:right-associate
	syntax:left-associate
	(macro RA)
	(macro LA)
	compose**
	(macro compose)
	(macro compose*)
	(macro compose//)
	true/0
	true/1
	false/0
	false/1
	just?
	maybe-function   (macro maybe)
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
 > (syntax:right-associate 'comp-function (list 'a 'b 'c) error)
 (comp-function a (comp-function b c))
 > (syntax:right-associate 'comp-function (list 'b 'c) error)
 (comp-function b c)

 ;; and left...
 > (syntax:left-associate 'comp-function (list 'a 'b 'c) error)
 (comp-function (comp-function a b) c)
 > (syntax:left-associate 'comp-function (list 'b 'c) error)
 (comp-function b c)

 ;; and errors:
 > (map (lambda (_)
	  (with-exception-catcher error-exception-message
				  (lambda () (_ 'comp-function (list 'c) error))))
	(list syntax:right-associate syntax:left-associate))
 (c c)
 > (map (lambda (_)
	  (with-exception-catcher error-exception-message
				  (lambda () (_ 'comp-function (list) error))))
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
 > (expansion#RA compose half x*y inc2values)
 (compose half (compose x*y inc2values))
 > (expansion#LA compose half x*y inc2values)
 (compose (compose half x*y) inc2values)
 )

;; as a function:

;; XX rename to compose*, or compose to compose* (no) or ?
(define (compose** . fs)
  (right-associate compose-function fs error))

;; as macro for more performance, unary only:
(define-macro* (compose . es)
  (early-bind-expressions
   es
   (with-gensym V
		`(lambda (,V)
		   ,(fold-right (lambda (e inner)
				  `(,e ,inner))
				V
				es)))))

;; same thing, n-ary: -- COPY PASTE
(define-macro* (compose* . es)
  (let* ((es* (map (lambda (e)
		     (let ((e* (source-code e)))
		       ;; pre-eval-sym, e 
		       (list (if (symbol? e*)
				 #f
				 (gensym)) e)))
		   es))
	 (lam (with-gensym VS
			   `(lambda ,VS
			      ,(let-pair
				((e0 er) (reverse es*))
				(fold (lambda (e inner)
					`(,(or (car e)
					       (cadr e)) ,inner))
				      `(apply ,(or (car e0)
						   (cadr e0)) ,VS)
				      er)))))
	 (es*-pre-eval (filter car es*)))
    (if (null? es*-pre-eval)
	lam
	`(let ,es*-pre-eval
	   ,lam))))

;; with parametrizable arity:
(define-macro* (compose// n . es)
  (early-bind-expressions
   es
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
		    (error "bug")))))))


(TEST
 > (define TEST:equal? syntax-equal?)

 > (expansion#compose a b c)
 (lambda (GEN:X-3566) (a (b (c GEN:X-3566))))
 > (expansion#compose a)
 (lambda (GEN:X-3567) (a GEN:X-3567))

 > (expansion#compose a (maybe b) (complement c))
 (##let ((GEN:-546 (maybe b)) (GEN:-547 (complement c)))
   (lambda (GEN:V-548) (a (GEN:-546 (GEN:-547 GEN:V-548)))))

 > (expansion#compose// 1 a b c)
 (lambda (GEN:-3382) (a (b (c GEN:-3382))))
 > (expansion#compose// 3 a b c)
 (lambda (GEN:-3383 GEN:-3384 GEN:-3385)
   (a (b (c GEN:-3383 GEN:-3384 GEN:-3385)))))



(TEST
 > (define (half x) (/ x 2))
 > (define (square x) (* x x))
 > (define x*y (lambda-values ((x y)) (* x y)))
 > ((compose** half inc-function square) 10)
 101/2
 > ((compose half inc square) 10)
 101/2
 > ((compose// 1 half inc square) 10)
 101/2

 > ((compose** half inc-function x*y) (values 10 20))
 201/2
 > ((compose half inc-function x*y) (values 10 20))
 201/2
 > ((compose// 1 half inc-function x*y) (values 10 20))
 201/2

 > (define (inc2values x y) (values (inc x) (inc y)))
 > ((compose** half x*y inc2values) 10 20)
 231/2
 > ((compose* half x*y inc2values) 10 20)
 231/2
 > ((compose// 2 half x*y inc2values) 10 20)
 231/2

 ;; compose is ("fully") associative (left or right doesn't matter),
 ;; so choosing right-associate was arbitrary
 > ((compose-function (compose half x*y) inc2values) 10 20)
 231/2
 > ((compose-function half (compose-function x*y inc2values)) 10 20)
 231/2)


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

;; function variant of (maybe pred), now that maybe is a macro
(define (maybe-function pred)
  (either not pred))

(define-macro* (maybe pred)
  (early-bind-expressions
   (pred)
   `(either not ,pred)))

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
  (compose complement-2ary flip))

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
 #f ;; ditto
 > ((sorted-list-of number? >) '())
 #t)


