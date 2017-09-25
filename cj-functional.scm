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

(export flip
	complement
	compose
	maybe-compose
	either (macro %either)
	neither (macro %neither)
	both
	all-of
	right-associate
	left-associate
	syntax:right-associate
	syntax:left-associate
	(macro RA)
	(macro LA)
	compose**
	(macro compose*)
	true/0
	true/1
	false/0
	false/1
	just?
	maybe
	on* ;; for binary |on| see cj-env
	(macro or**)
	(macro or/)
	list-of
	list-of/length ;; see also length-is
	improper-list/length>=
	pair-of
	strictly-monotonic-list-of
	<to<=
	sorted-list-of
	values-of
	applying)



(define (flip f)
  (lambda (x y)
    (f y x)))

(define (complement fn)
  (lambda v
    (not (apply fn v))))

(define (compose f g)
  (lambda x
    (f (apply g x))))

(define (maybe-compose f g)
  (lambda x
    (cond ((apply g x) => f)
	  (else #f))))

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

;; macro version of either, not (only) for performance, but for late
;; binding:
(define-macro* (%either . fs)
  (with-gensym
   V
   `(lambda (,V)
      (or ,@(map (lambda (f)
		   `(,f ,V))
		 fs)))))

(TEST ;; copy of test cases above
 > ((%either symbol? string?) "foo")
 #t
 > ((%either symbol? string?) 'bar)
 #t
 > ((%either symbol? string?) 0)
 #f
 > ((%either symbol? number? string?) 0)
 #t
 ;; test shortcutting?
 )


(define (neither . fs)
  (complement (apply either fs)))

(define-macro* (%neither . fs)
  (with-gensym
   V
   `(lambda (,V)
      (not (or ,@(map (lambda (f)
			`(,f ,V))
		      fs))))))

(TEST ;; copy of test cases above
 > ((neither symbol? string?) "foo")
 #f
 > ((neither symbol? string?) 'bar)
 #f
 > ((neither symbol? string?) 0)
 #t
 > ((neither symbol? number? string?) 0)
 #f
 ;; test shortcutting?
 )

(TEST ;; copy of test cases above
 > ((%neither symbol? string?) "foo")
 #f
 > ((%neither symbol? string?) 'bar)
 #f
 > ((%neither symbol? string?) 0)
 #t
 > ((%neither symbol? number? string?) 0)
 #f
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
 > (expansion#RA compose half x*y inc2values)
 (compose half (compose x*y inc2values))
 > (expansion#LA compose half x*y inc2values)
 (compose (compose half x*y) inc2values)
 )

;; as a function:

(define (compose** . fs)
  (right-associate compose fs error))

;; as macro for a tad more performance:

(IF #t
    (define-macro* (compose* . f-exprs)
      `(RA compose ,@f-exprs))
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

(define (list-of pred)
  (lambda (x)
    (and (list? x)
	 (every pred x))))

;; see also length-is
(define (list-of/length pred len)
  (lambda (val)
    (let lp ((n len)
	     (v val))
      (if (zero? n)
	  (null? v)
	  (and (pair? v)
	       (pred (car v))
	       (lp (dec n)
		   (cdr v)))))))

(TEST
 > (map (list-of/length integer? 2)
	'((1 2)
	  (1.1 2)
	  (1)
	  ()
	  (1 2 3)
	  (a b)))
 (#t #f #f #f #f #f))

(define (improper-list/length>= len)
  (lambda (v)
    (let lp ((n len)
	     (v v))
      (if (zero? n)
	  #t
	  (FV (v)
	      (and (pair? v)
		   (lp (dec n)
		       (cdr v))))))))

(TEST
 > (define l '(a
	       (a)
	       (a . b)
	       (a b)
	       (a b . c)
	       (a b c)))
 > (map (improper-list/length>= 0) l)
 (#t #t #t #t #t #t)
 > (map (improper-list/length>= 1) l)
 (#f #t #t #t #t #t)
 > (map (improper-list/length>= 2) l)
 (#f #f #f #t #t #t))

(define (pair-of t1? t2?)
  (lambda (v)
    (and (pair? v)
	 (t1? (car v))
	 (t2? (cdr v)))))

(define (strictly-monotonic-list-of el? <)
  (lambda (v)
    (or (null? v)
	(and (pair? v)
	     (let next ((v (cdr v))
			(last-a (car v)))
	       (or (null? v)
		   (and (pair? v)
			(let-pair ((a v*) v)
				  (and (el? a)
				       (< last-a a)
				       (next v* a))))))))))

(TEST
 > ((strictly-monotonic-list-of number? <) '(1 2 3))
 #t
 > ((strictly-monotonic-list-of number? <) '(3 2 1))
 #f
 > ((strictly-monotonic-list-of number? >) '(3 2 1))
 #t
 > ((strictly-monotonic-list-of number? >) '(3 2 2))
 #f
 > ((strictly-monotonic-list-of number? >=) '(3 2 2))
 #t
 > ((strictly-monotonic-list-of number? <) '(1 2 2))
 #f
 > ((strictly-monotonic-list-of number? <=) '(1 2 2))
 #t
 > ((strictly-monotonic-list-of number? >=) '())
 #t)


(define <to<=
  ;; <-><= would be a fun name, wouldn't it?
  (compose complement flip))

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


(define (values-of . preds)
  (let ((len (length preds)))
    (if (= len 1)
	(car preds)
	(lambda (v)
	  (and (values? v)
	       (let ((vals (values->list v)))
		 (and (= (length vals) len)
		      (every (lambda (val pred)
			       (pred val))
			     vals
			     preds))))))))

(TEST
 > ((values-of boolean? string?) (values #f ""))
 #t
 > ((values-of boolean? string?) (values #f))
 #f
 > ((values-of boolean?) (values #f))
 #t
 > ((values-of) (values))
 #t
 > ((values-of) (values 1 2))
 #f
 > ((values-of integer? number?) (values 1.4 2))
 #f
 > ((values-of integer? number?) (values 2 1.4))
 #t
 )


;; name?
(define (applying fn)
  (lambda (args)
    (apply fn args)))

;; should it take optional args to insert before |args|? Or leave that
;; to usage of cut?

