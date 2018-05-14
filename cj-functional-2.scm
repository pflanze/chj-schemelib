;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (cj-source source-error)
	 (list-util let-pair)
	 (list-util-1 map/iota)
	 (cj-symbol with-gensym)
	 (code-util early-bind-expressions)
	 ;; for tests:
	 test
	 (fixnum inc dec)
	 srfi-1
	 (cj-symbol syntax-equal?))

(export flip-function        (macro flip)
	complement-function  (macro complement complement-2ary)
	compose-function
	maybe-compose
	either-function      (macro either)
	neither-function     (macro neither)
	both-function        (macro both)
	all-of-function      (macro all-of)
	(macro =>)
	(macro =>*)
	exact-natural0? ;; can't be in predicates-1 for dependency reasons
	(macro =>-lambda)
	(macro =>-lambda/arity)
	(macro =>>)
	(macro =>>*)
	list-of/2
	list-of-function (macro list-of)
	nonempty-list-of-function (macro nonempty-list-of)
	list-of/length ;; see also length-is
	improper-list/length>=
	pair-of-function (macro pair-of)
	strictly-monotonic-list-of
	values-of-function (macro values-of)
	applying)

(include "cj-standarddeclares.scm")


(define (flip-function f)
  (lambda (x y)
    (f y x)))

(define-macro* (flip f)
  (early-bind-expressions
   (f)
   (with-gensyms
    (x y)
    `(##lambda (,x ,y)
	  (,f ,y ,x)))))


(define (complement-function fn)
  (lambda v
    (not (apply fn v))))

(define-macro* (complement fn)
  (early-bind-expressions
   (fn)
   (with-gensym
    V
    `(lambda (,V)
       (not (,fn ,V))))))

(define-macro* (complement-2ary fn)
  (early-bind-expressions
   (fn)
   (with-gensyms
    (V1 V2)
    `(lambda (,V1 ,V2)
       (not (,fn ,V1 ,V2))))))

(define (compose-function f g)
  (lambda x
    (f (apply g x))))

(define (maybe-compose f g)
  (lambda x
    (cond ((apply g x) => f)
	  (else #f))))

(define (either-function . fs)
  (if (null? fs)
      (lambda x
	#f)
      (let-pair ((f fs*) fs)
		((lambda (r)
		   (lambda x
		     (or (apply f x)
			 (apply r x))))
		 (apply either-function fs*)))))

(TEST
 > ((either-function symbol? string?) "foo")
 #t
 > ((either-function symbol? string?) 'bar)
 #t
 > ((either-function symbol? string?) 0)
 #f
 > ((either-function symbol? number? string?) 0)
 #t
 ;; test shortcutting?
 )

(define-macro* (either . fs)
  (early-bind-expressions
   fs
   (with-gensym
    V
    `(lambda (,V)
       (or ,@(map (lambda (f)
		    `(,f ,V))
		  fs))))))

(TEST ;; copy of test cases above
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


(define (neither-function . fs)
  (complement (apply either-function fs)))

(define-macro* (neither . fs)
  (with-gensym
   V
   `(complement (either ,@fs))))

(TEST ;; copy of test cases above
 > ((neither-function symbol? string?) "foo")
 #f
 > ((neither-function symbol? string?) 'bar)
 #f
 > ((neither-function symbol? string?) 0)
 #t
 > ((neither-function symbol? number? string?) 0)
 #f
 ;; test shortcutting?
 )

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



;; name ok?
(define (both-function f0 f1)
  (lambda x
    (and (apply f0 x)
	 (apply f1 x))))

(TEST
 > ((both-function even? odd?) 1)
 #f
 > ((both-function even? odd?) 2)
 #f
 > ((both-function even? negative?) 2)
 #f
 > ((both-function even? negative?) -2)
 #t
 > ((both-function even? negative?) -1)
 #f
 )

;; name?
(define (all-of-function . preds)
  (lambda x
    (every (lambda (pred)
	     (apply pred x))
	   preds)))

(TEST
 > ((all-of-function even? odd?) 1)
 #f
 > ((all-of-function even? odd?) 2)
 #f
 > ((all-of-function even? negative?) 2)
 #f
 > ((all-of-function even? negative?) -2)
 #t
 > ((all-of-function even? negative?) -1)
 #f

 > ((all-of-function odd?) 1)
 #t
 > ((all-of-function) 1)
 #t
 )

(define-macro* (all-of . preds)
  (early-bind-expressions
   preds
   (with-gensym
    V
    `(lambda (,V)
       (and ,@(map (lambda (pred)
		     `(,pred ,V))
		   preds))))))

(TEST ;; copy of test cases above
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
 ;; wow didn't expect that, but ok makes sense, (and) resolves to #t,
 ;; nice, but wow didn't expect the interpreter to resolve that,
 ;; either:
 ;;
 ;;  > (expansion (all-of))
 ;;  (lambda (GEN:V-686) #t)
 ;;  > (expansion#%all-of)
 ;;  (lambda (GEN:V-687) (and))
 )


(define-macro* (both a b)
  `(all-of ,a ,b))



;; The "Clojure-macros". Clojure calls them -> and ->> instead, but ->
;; is taken by cj-typed. Called this "chain" previously (but then how
;; to call the other variant, chain> ?)

;; Also see source.=> in code-cj-functional.scm

(define (=>-expand start exprs)
  (let next ((exprs exprs)
	     (res start))
    (if (null? exprs)
	res
	(let-pair ((expr exprs*) exprs)
		  (next exprs*
			(let ((expr* (source-code expr))
			      (src (lambda (e)
				     (possibly-sourcify e expr))))
			  (cond
			   ((pair? expr*)
			    (src `(,(car expr*) ,res ,@(cdr expr*))))
			   ((symbol? expr*)
			    (src `(,expr ,res)))
			   (else
			    (source-error
			     expr
			     "expecting a form or a symbol")))))))))

(TEST
 > (=>-expand 'input '((foo-set 1) (bar-set 2)))
 (bar-set (foo-set input 1) 2))

(define-macro* (=> start . exprs)
  (=>-expand start exprs))

(define-macro* (=>* expr0 . exprs)
  (with-gensym
   V
   (if (symbol? (source-code expr0))
       `(##lambda ,V
		  ,(=>-expand (possibly-sourcify `(##apply ,expr0 ,V) expr0)
			      exprs))
       ;; otherwise can't support multiple values:
       `(##lambda (,V)
		  ,(=>-expand V (cons expr0 exprs))))))

(TEST
 > ((=>* (inc)) 10)
 11
 > ((=>-lambda inc inc) 10)
 12
 ;; multiple arguments:
 > ((=>* + inc))
 1
 > ((=>* + inc) 2 3)
 6
 > (with-exception-catcher wrong-number-of-arguments-exception?
			   (lambda () ((=>* (+) inc) 2 3)))
 #t
 > ((=>* (+) inc) 2)
 3)


;; always 1-ary, OK? XX or change =>* to this, how is Clojure dealing
;; with this?
(define-macro* (=>-lambda expr0 . exprs)
  (with-gensym
   V
   `(##lambda (,V)
	      ,(=>-expand V (cons expr0 exprs)))))


(define exact-natural0? (both natural0? exact?))

(define-macro* (=>-lambda/arity n expr0 . exprs)
  (let ((n* (eval n)))
    (if (exact-natural0? n*)
	(let ((VS (map (lambda (i) (gensym))
		       (iota n*))))
	  `(##lambda ,VS
		     ,(=>-expand (possibly-sourcify `(,expr0 ,@VS) expr0)
				 exprs)))
	(source-error n "expecting expression evaluating to natural0"))))

(TEST
 > ((=>-lambda car string) '(#\a #\b))
 "a"
 > (with-exception-catcher wrong-number-of-arguments-exception?
			   (lambda () ((=>-lambda car string) '(#\a #\b) 3)))
 #t
 > ((=>-lambda/arity 1 car string) '(#\a #\b))
 "a"

 > ((=>-lambda ((lambda (x) #\y)) string) '(#\a #\b))
 "y"
 > ((=>-lambda/arity 1 (lambda (x) #\y) string) '(#\a #\b))
 "y"
 ;; ^ unlike =>-lambda, the first expression does *not* need an
 ;; additional paren wrap! XX messy, what to do?
 )

(TEST
 > (define TEST:equal? syntax-equal?)
 
 > (expansion#=>-lambda ((lambda (x) #\y)) string)
 (##lambda (GEN:V-668) (string ((lambda (x) #\y) GEN:V-668)))
 > (expansion#=>-lambda car string)
 (##lambda (GEN:V-671) (string (car GEN:V-671)))

 > (expansion#=>-lambda/arity 1 (lambda (x) #\y) string)
 (##lambda (GEN:-672) (string ((lambda (x) #\y) GEN:-672)))
 > (expansion#=>-lambda/arity 0 (lambda (x) #\y) string)
 (##lambda () (string ((lambda (x) #\y))))
 > (expansion#=>-lambda/arity 2 (lambda (x y) #\y) string)
 (##lambda (GEN:-1 GEN:-2) (string ((lambda (x y) #\y) GEN:-1 GEN:-2)))

 > (expansion#=>-lambda/arity 1 e0 e1 e2)
 (##lambda (GEN:-723) (e2 (e1 (e0 GEN:-723))))
 > (expansion#=>-lambda/arity 1 (e0) e1 e2)
 (##lambda (GEN:-724) (e2 (e1 ((e0) GEN:-724))))
 > (expansion#=>-lambda/arity 1 e0 (e1) e2)
 (##lambda (GEN:-725) (e2 (e1 (e0 GEN:-725))))

 ;; Compared to =>* :
 ;; currently this is the same with non-symbol expressions:
 > (expansion#=>* ((lambda (x) #\y)) string)
 (##lambda (GEN:V-669) (string ((lambda (x) #\y) GEN:V-669)))
 ;;   ^ BTW it does *not* evaluate expressions once-only like on,
 ;;     comp, either do.
 ;; but not this:
 > (expansion#=>* car string)
 (##lambda GEN:V-670 (string (##apply car GEN:V-670))))



;; bah, copy-paste except for one line
(define (=>>-expand start exprs)
  (let next ((exprs exprs)
	     (res start))
    (if (null? exprs)
	res
	(let-pair ((expr exprs*) exprs)
		  (next exprs*
			(let ((expr* (source-code expr))
			      (src (lambda (e)
				     (possibly-sourcify e expr))))
			  (cond
			   ((pair? expr*)
			    ;; only change here:
			    (src `(,(car expr*) ,@(cdr expr*) ,res)))
			   ((symbol? expr*)
			    (src `(,expr ,res)))
			   (else
			    (source-error
			     expr
			     "expecting a form or a symbol")))))))))


;; dito
(define-macro* (=>> start . exprs)
  (=>>-expand start exprs))

(define-macro* (=>>* expr0 . exprs)
  (with-gensym
   V
   (if (symbol? (source-code expr0))
       `(##lambda ,V
		  ,(=>>-expand (possibly-sourcify `(##apply ,expr0 ,V) expr0)
			       exprs))
       ;; otherwise can't support multiple values:
       `(##lambda (,V)
		  ,(=>>-expand V (cons expr0 exprs))))))

;; it's actually REALLY all copy-paste except for =>>-expand call,
;; which is a function, bah.todo.


(TEST
 > (=> (=>> (iota 10)
	    (map inc-function)
	    (filter even?))
       (take 2))
 (2 4)
 > ((=>>* (inc)) 10)
 11
 > ((=>>* inc-function (inc) inc inc) 10)
 14
 ;; multiple arguments:
 > (with-exception-catcher divide-by-zero-exception?
			   (lambda () ((=>>* + (/ 2)))))
 #t
 > ((=>>* + (/ 2)) 2 3)
 2/5
 > (with-exception-catcher wrong-number-of-arguments-exception?
			   (lambda () ((=>>* (+) inc) 2 3)))
 #t
 > ((=>>* (+) inc) 2)
 3)


(define (list-of/2 pred v)
  (let lp ((v v))
    (cond ((pair? v)
	   (and (pred (car v))
		(lp (cdr v))))
	  ((null? v)
	   #t)
	  (else
	   #f))))

(define list-of-function (C list-of/2 _))

(define-macro* (list-of pred)
  ;; (this is still a tad more than just inlining: late binding still
  ;; provided. Which will allow the compiler to lambda lift the generated
  ;; function (in cases where it can't be inlined itself))
  (early-bind-expressions
   (pred)
   (with-gensym x
		`(##lambda (,x)
			   (list-of/2 ,pred ,x)))))


(define (nonempty-list-of-function pred)
  (both pair? (list-of pred)))

(define-macro* (nonempty-list-of pred)
  (early-bind-expressions
   (pred)
   `(both pair? (list-of ,pred))))


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

(define (pair-of-function t1? t2?)
  (lambda (v)
    (and (pair? v)
	 (t1? (car v))
	 (t2? (cdr v)))))

(define-macro* (pair-of t1? t2?)
  (early-bind-expressions
   (t1? t2?)
   (with-gensym
    v
    `(##lambda (,v)
	  (##and (##pair? ,v)
		 (,t1? (##car ,v))
		 (,t2? (##cdr ,v)))))))

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



(define (values-of-function . preds)
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

(define-macro* (values-of . preds)
  (let ((len (length preds)))
    (if (not (fixnum? len))
	(error "bug"))
    (if (= len 1)
	(car preds)
	(with-gensym
	 V
	 `(lambda (,V)
	    (and (##values? ,V)
		 (##fx= (##vector-length ,V) ,len)
		 ,@(map/iota (lambda (pred i)
			       `(,pred (##vector-ref ,V ,i)))
			     preds)))))))

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
 > ((values-of integer? number?) (values 2 1.4 3))
 #f)


;; name?
(define (applying fn)
  (lambda (args)
    (apply fn args)))

;; should it take optional args to insert before |args|? Or leave that
;; to usage of cut?

