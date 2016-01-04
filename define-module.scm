;;; Copyright 2010-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.cj-phasing)
	 (lib.test)
	 (lib.simple-match)
	 (lib.srfi-11))


;;;
;;;; runtime-parameterized modules
;;;

;; define-macro* is supported, both for making and use of definitions;
;; module parameters are runtime only (macro expander code can't
;; access them)

;; XXX does not handle lexical scope, e.g. macro bindings are never
;; shadowed, neither is other syntax, like in (let ((let 'a)) let)
;; which should return a


(both-times ;; runtime mostly just for the tests

 ;; lib
 (define source-xxone
   (lambda (x) (xone x (lambda (e) (source-error x "expecting one item")))))
 ;; /lib

 ;; % for (mutable/)mutated-from-inside data structures?
 (define (define-module:redo *local-expanders %seen-var)
   (named
    redo
    (lambda (form movedout+res)
      (let* ((return
	      (lambda (val)
		(letv ((movedout res) movedout+res)
		      (values movedout
			      (cons val res)))))

	     (redo-with
	      (lambda (val)
		(redo val movedout+res)))

	     (return-non-define
	      (lambda (form)
		(return (list (gensym)
			      form))))

	     (move-out
	      (lambda (val)
		(letv ((movedout res) movedout+res)
		      (values (cons val movedout)
			      res)))))

	(let ((form* (source-code form)))
	  (if (pair? form*)
	      (let-pair
	       ((head rest1) form*)
	       (let ((head* (source-code head)))
		 (if (symbol? head*)
		     (case head*
		       ((define)
			(match* ;; is a define never a dotted list? XX
			 form
			 ((_define bind . rest2)

			  (define (return-definition name expr)
			    (if (table-ref %seen-var
					   (source-code name)
					   #f)
				;; translate into a set!
				(return-non-define
				 `(set! ,name ,expr))
				;; first occurrence,
				;; introduce new binding
				(begin
				  (table-set! %seen-var
					      (source-code name)
					      #t)
				  (return (list name expr)))))

			  ;; what kind of define form?
			  (let ((bind* (source-code bind)))
			    (cond ((pair? bind*)
				   ;; function definition
				   (let-pair
				    ((name vars) bind*)
				    (return-definition
				     name
				     `(lambda ,vars
					,@rest2))))
				  ((symbol? bind*)
				   ;; variable definition
				   (return-definition bind (source-xxone rest2)))
				  (else
				   (source-error
				    bind
				    "expecting pair or symbol")))))))
		       ((begin)
			;; flatten into the outer list
			(fold redo
			      movedout+res
			      rest1))
		       ((##define-syntax)
			(move-out form))
		       (else
			(cond ((or (cond ((assq head* (unbox *local-expanders))
					  => cdr)
					 (else #f))
				   (define-macro-star-maybe-ref head*))
			       => (lambda (expand)
				    ;;(warn "found expander for" head*)
				    (redo-with (expand form))))
			      (else
			       (return-non-define form)))))
		     (return-non-define form))))
	      (return-non-define form)))))))

 (TEST
  > (define TEST:equal? syntax-equal?)
  > (map (lambda (form)
	   (values->vector ((define-module:redo (box '()) (make-table))
			    form
			    (values '() '()))))
	 (list '(foo x)
	       '(set! foo x)
	       '(define foo x)))
  (
   #(() ((GEN:-5888 (foo x))))
   #(() ((GEN:-5889 (set! foo x))))
   #(() ((foo x)))))


 (define (define-module:convert-forms forms *local-expanders)
   (define t (make-table))
   ;; use left fold then reverse so that the order of the side
   ;; effects done by the macro expanders is correct
   (letv ((revmovedout revconvertedforms)
	  (fold (define-module:redo *local-expanders t)
		(values '() '())
		forms))
	 (values (reverse revconvertedforms)
		 (reverse revmovedout))))

 (TEST
  > (define TEST:equal? syntax-equal?)
  > (values->vector (define-module:convert-forms
		      '((define a 1)
			(set! a 2)
			(##define-syntax X foo)
			(define a 3))
		      (box '())))
  #( ;; convertedforms
    ((a 1)
     (GEN:1 (set! a 2))
     (GEN:2 (set! a 3)))
    ;; movedout
    ((##define-syntax X foo))))
 
 (define (convert-module-body forms bodytail)

   (define *local-expanders (box '()))

   (parameterize
    ((define-macro*-maybe-local-callback
       (lambda (name expander)
	 (push! (unbox *local-expanders) (cons name expander)))))
    
    (letv ((convertedforms movedout)
	   (define-module:convert-forms forms *local-expanders))

	  (if (mod:compiled?)
	      `(begin
		 ,@movedout
		 (letrec ,convertedforms
		   ,@bodytail))
	      `(let ,(map (lambda (var+expr)
			    (match* var+expr
				    ((var expr)
				     `(,var 'define-module-unbound))))
			  (filter (compose* not cj-gensym? car)
				  convertedforms))
		 ,@movedout
		 ,@(map (lambda (var+expr)
			  (match* var+expr
				  ((var expr)
				   (if (cj-gensym? var)
				       expr
				       `(set! ,var ,expr)))))
			convertedforms)
		 ,@bodytail))))))


(TEST
 > (require (lib.cj-symbol)))
(TEST
 > (define TEST:equal? syntax-equal?)
 > (define (conv forms body)
     (vector
      (convert-module-body forms body)
      (parameterize
       ((mod:compiled? #t))
       (convert-module-body forms body))))
 > (conv '((define a 1) (define a 12) (define b (a 2))) '(mybody))
 #((let ((a 'define-module-unbound)
	 (b 'define-module-unbound))
     (set! a 1)
     (set! a 12)
     (set! b (a 2))
     mybody)
   (begin (letrec ((a 1) (GEN:1 (set! a 12)) (b (a 2))) mybody)))
 > (conv '((define a 1) (set! a list) (define b (a 2))) '(b))
 #((let ((a 'define-module-unbound) (b 'define-module-unbound))
     (set! a 1) (set! a list) (set! b (a 2))
     b)
   (begin (letrec ((a 1) (GEN:3716 (set! a list)) (b (a 2))) b)))
 > (eval (vector-ref # 0))
 (2)
 ;; macro expansion test see further below (expansion too big to use here)
 )


(define-macro* (define-module name-or-name+params export-form . body)
  (assert* (either pair? symbol?) name-or-name+params
	   (lambda (name-or-name+params*)
	     ((lambda (name)
		(match*
		 export-form
		 ((_export . exports)
		  (if (eq? (source-code _export) 'export)
		      (with-gensyms
		       (VARNAME)
		       (let ((exports-name (symbol-append name '-exports)))
			 `(begin
			    (define ,exports-name
			      ',exports)
			    (define ,name-or-name+params
			      ,(convert-module-body
				body
				`((lambda (,VARNAME)
				    (if ,VARNAME
					(case ,VARNAME
					  ,@(map/tail
					     (lambda_
					      `((,_) ,_))
					     `((else
						(error
						 "in module, name not exported:"
						 ',name
						 ,VARNAME)))
					     (source-code exports)))
					,exports-name))))))))
		      (source-error
		       export-form
		       "expecting (export . VAR*) form")))))
	      (if (pair? name-or-name+params*)
		  (car name-or-name+params*)
		  name-or-name+params*)))))

(TEST
 > (define-module (foo x) (export f) (define (f n) (/ n x)))
 > (((foo 10) 'f) 5)
 1/2
 > ((foo 10) #f)
 (f)
 )


(define (module:import-expand DEFINE expr vars)
  (let ((mk (lambda (select)
	      (lambda_
	       (if (symbol? (source-code _))
		   _
		   (match-list* _
				((to from) (select from to))))))))
    (let ((from (mk (lambda (from to) from)))
	  (to (mk (lambda (from to) to))))
      
      (if (pair? vars)
	  (with-gensyms
	   (M)
	   `(begin
	      ,@(map (lambda_
		      `(,DEFINE ,(to _) #f))
		     vars)
	      (let ((,M ,expr))
		,@(map (lambda_
			;; (rather inefficient, allocates closures for all of
			;; the function variables)
			`(set! ,(to _) (,M ',(from _))))
		       vars))))
	  (source-error
	   stx
	   "expecting a list of variables to import after the first argument")))))

(define-macro* (module:import expr . vars)
  (module:import-expand 'define expr vars))

(TEST
 > (module:import (foo 11) f)
 > (f 4)
 4/11

 ;; macro test
 > (define-module (tmod)
     (export f expander#tmac)
     (define-macro* (tmac x)
       (list 'quote x))
     (define (f . a)
       (cons (tmac foo) a)))
 > (module:import (tmod) f)
 > (f 1 2)
 (foo 1 2)
 ;; > tmac
 ;; *** ERROR IN (console)@17.1 -- Macro name can't be used as a variable: tmac
 ;; hmm hu
 > (define-macro-star-maybe-ref 'tmac)
 #f
 ;; > expander#tmac
 ;; *** ERROR IN (console)@3.1 -- Unbound variable: expander#tmac
 > (module:import (tmod) expander#tmac)
 > (procedure? expander#tmac)
 #t

 ;; more macro testing: (use leading to def and then use the latter too)
 > (define-module (tmod2 x)
     (export b make-foo foo incfoobar)
     (define a 1)     
     (define-struct foo bar)
     (set! a list)
     (define b (a x))
     (define foo make-foo)
     (define (incfoobar x)
       (let-foo ((b) x)
		(inc b))))
 > (module:import (tmod2 'A) foo make-foo incfoobar b)
 > b
 (A)
 > (eq? foo make-foo)
 #t
 > (incfoobar (make-foo 10))
 11

 ;; renaming
 > (module:import (tmod2 'A2) (bbb b))
 > bbb
 (A2)
 )


(define (module:parse-prefix prefix cont)
  (let ((prefix* (source-code prefix)))
    (cond ((symbol? prefix*)
	   (cont prefix*))
	  ((keyword? prefix*)
	   (cont (symbol-append (keyword->string prefix*) ":")))
	  (else
	   (source-error prefix "expecting string or symbol")))))

(define (module:import/prefix-expand DEFINE expr prefix vars)
  (if (null? vars)
      (source-error ctx "missing bindings to import")
      (module:parse-prefix
       prefix
       (lambda (prefix)
	 (module:import-expand
	  DEFINE
	  expr
	  (map (lambda (var)
		 (assert* symbol? var
			  (lambda (var)
			    `(,(symbol-append prefix var) ,var))))
	       vars))))))

(define-macro* (module:import/prefix expr prefix . vars)
  (module:import/prefix-expand 'define expr prefix vars))

(TEST
 > (define-module (foo x) (export a b c) (define a 4))
 > (module:import/prefix (foo 4) foo: a b)
 > foo:a
 4
 )


;; *Can't* write a |module:import-all/prefix| because the module
;; initialization expression is evaluated at runtime (thus it's not
;; even known what module it will be). Those are runtime modules
;; really, after all...

;; But can write a macro that builds up the initialization expression
;; itself:

;; NOTE: use of module-import implies that the module accepts the
;; prefix as its first argument!

(define (module-import-expand DEFINE pass-prefix?
			      prefix name args)
  (assert* symbol? name
	   (lambda (name)
	     (module:import/prefix-expand
	      DEFINE
	      `(,name
		,@(if pass-prefix?
		      `(',prefix)
		      `())
		,@args)
	      prefix
	      (eval (symbol-append name '-exports))))))

(define-macro* (module-import prefix name . args)
  (module-import-expand 'define #t prefix name args))

;; module-import* does not implicitly pass the prefix

(define-macro* (module-import* prefix name . args)
  (module-import-expand 'define #f prefix name args))

(TEST
 > (compile-time ;; necessary since TEST evaluates all subtests in one go
    (define-module (foo prefix x) (export a b)
      (define a -3)
      (define a 4)
      (define b (* x a))))
 > (module-import foo5: foo 5)
 > foo5:a
 4
 > foo5:b
 20
 > (module-import* foo6: foo #f 6)
 > foo6:b
 24
 )


;; ------------------------------------------------------------------
;; Only works with dot-oo:

(define-macro* (module-import. prefix name . args)
  (module-import-expand 'define. #t prefix name args))

(define-macro* (module-import.* prefix name . args)
  (module-import-expand 'define. #f prefix name args))

