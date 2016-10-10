;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 predicates-1 ;; for re-exports only?
	 cj-typed
	 dot-oo
	 symboltable-1
	 (cj-env keyword->symbol))


(export box-of
	forced ;; rename to possibly-promise-of ?
	any? true/1
	false/2
	false? ;; == not
	true?
	true
	nonnegative-real?
	nonpositive-real?
	negative-real?
	inexact-real?
	exact-real?
	exact-integer?
	exact-number?
	pair-or-null?
	pair-with-car
	nonempty-string?
	improper*-map/tail ;; XX move
	improper*-map	   ;; dito
	string-of
	string-of-length
	improper-every	  ;; XX move
	improper-list-of  ;; hmm
	char-one-of	  ;; move to char lib?
	perhaps-source-of ;; XX rename to possibly-source-of ?
	source-of
	perhaps-source*-of ;; dito
	source*-of
	length-=
	length-<=
	length->=
	length-is ;; see also list-of/length  -- rename to list-of-length ?
	length=
	list-of-length
	lists?
	0..1? ;; see also rgb:0..1?
	in-signed-range?
	parameter?

	;; and the new meat:
	function?
	predicate?
	throwing
	function-of
	procedure-of
	arguments-of)


;; for now there's no difference (intent: pure functions, aside of
;; unsafe I/O through stderr, ok? No further typing then, either.)

(define function? procedure?)

;; ^ also, for (java etc.) translation, more information of course,
;; finally.

;; well, like:

;; a function that takes any value and returns a boolean.
(define predicate? function?)

;; XX this must be part of cj-typed of course if it is to be a
;; non-noop
(define-typed (throwing #(predicate? e?)
			#(predicate? t?))
  t?)



(define-typed (function-of #(predicate? inputs?) #(predicate? output?))
  ;; (lambda vals
  ;;   (if (inputs? vals)
  ;; 	(let ((res ())))))

  ;; (lambda (fn)
  ;;   )

  ;; Ah stupid (again?), can't do this. Predicates are only used to
  ;; check the input, not to wrap it. Can't make a type-checking
  ;; wrapped version of fn. Really need the static declarations and
  ;; checking. For now:
  function?)


(define procedure-of function-of)



;; (def (argument-key #(symbol? key)
;; 		   #(predicate? pred))
;;      (lambda (k v else)
;;        (if (eq? k key)
;; 	   (pred v)
;; 	   (else))))

;; but then also need to check whether it *is* a keyword argument, so,
;; use 'structs' after all. OH, can actually depend on them, wow. Ah,
;; even dot-oo, so that we can get type checking (shouldn't
;; define-typed-struct be an intermediate layer, though, for some
;; cases?) So:

(define-struct. argument-key
  #(symbol? key)
  #(predicate? pred))


;; no support for rest / optional yet, only key using the above
;; (Should I really instead just generate code?, a function with the
;; given syntax, that returns #t, and if there are exceptions, well,
;; catch them. That would support all features. But then all of this
;; is useless anyway, right, since can't wrap functions, it's
;; completely pointless until we get proper type objects and
;; checking. And then it will be somewhat different anyway, but *will*
;; need a proper implementation of the DSSSL stuff.)

(define (arguments-of . preds)
  (let* ((normalpreds (filter (lambda (v)
				(not (argument-key? v)))
			      preds))
	 (keypreds (filter argument-key? preds))
	 (maybe-keypredstable (and (pair? keypreds)
				   (list->symboltable
				    (map (lambda (kp)
					   (let-argument-key ((k p) kp)
							     (cons k p)))
					 keypreds)))))

    (lambda (args)

      (define (keyword-process arguments)
	(if (and (null? arguments)
		 (not maybe-keypredstable))
	    #t

	    (if maybe-keypredstable
		(let ((tbl (symboltable-copy maybe-keypredstable)))
		  (let lp ((as arguments))
		    (if (null? as)
			;; check the missing keyword parameters, whether they
			;; all accept a false, ah, or default value, oh. so
			;; complex.
			(symboltable:every?
			 tbl
			 (lambda (k pred)
			   ;; predicate accepts false, but XX should check
			   ;; that it doesn't have the default value;
			   ;; arguments-of doesn't support those, thus safe
			   ;; for now, but should extend.

			   ;; oh, symboltable:every? of course still
			   ;; iterates over all keys, since I didn't
			   ;; delete them. (Should symboltable have a
			   ;; delete! method that sets a special value
			   ;; into the value slot to indicate
			   ;; deletion?) (Stupid, I thought I had a
			   ;; nice optimal way to do things but, not
			   ;; so much.)
			   (if pred
			       (pred #f)
			       ;; pred was deleted, hence satisfied already
			       #t)))

			(let-pair
			 ((a as*) as)
			 (and (keyword? a)
			      (pair? as*)
			      (let ((k (keyword->symbol a)))
				(cond ((symboltable-ref tbl
							k
							#f)
				       ;; ^ values in tbl are also set
				       ;; to #f, which means,
				       ;; duplicate keywords are
				       ;; leading to #f for the whole
				       ;; arguments-of, which seems
				       ;; sensible since it's an error.
				       => (lambda (pred)
					    (let-pair
					     ((b as**) as*)
					     (and (pred b)
						  (begin
						    (symboltable-set! tbl
								      k
								      #f)
						    (lp as**))))))
				      (else #f))))))))
		;; superfluous arguments
		#f)))
      
      (let lp ((ps normalpreds)
	       (as args))
	(if (null? ps)
	    (keyword-process as)
	    (if (pair? as)
		;; now could save the length checks above!
		(let-pair ((p? ps*) ps)
			  (let-pair ((a as*) as)
				    (and (p? a)
					 (lp ps* as*))))
		(if (null? as)
		    #f
		    (error "improper args list"))))))))

(TEST
 > (def p? (arguments-of any?))
 > (p? '(1))
 #t
 > (p? '())
 #f
 > (p? '(1 2))
 #f
 > (p? '(""))
 #t
 > (def p? (arguments-of string? number?))
 > (p? '(""))
 #f
 > (p? '("" 1))
 #t
 > (p? '(1 ""))
 #f
 > (p? '(1 1))
 #f

 ;; and keywords:
 > (def p? (arguments-of symbol?
			 (argument-key 'foo (maybe string?))
			 (argument-key 'bar (maybe number?))))
 > (p? '(f))
 #t
 > (p? '())
 #f
 > (p? '(f foo: #f))
 #t
 > (p? '(f foo: #t))
 #f
 > (p? '(f foo: "foo"))
 #t
 > (p? '(f foo: "foo" bar: 13))
 #t
 > (p? '(f foo: "foo" bar: "13"))
 #f
 > (def p? (arguments-of symbol?
			 (argument-key 'foo (maybe string?))
			 (argument-key 'bar number?)))
 > (p? '(f))
 #f
 > (p? '())
 #f
 > (p? '(f foo: #f))
 #f
 > (p? '(f foo: #t))
 #f
 > (p? '(f foo: "foo"))
 #f
 > (p? '(f foo: "foo" bar: 13))
 #t
 > (p? '(f bar: 13))
 #t
 > (p? '(f foo: "foo" bar: "13"))
 #f
 )

