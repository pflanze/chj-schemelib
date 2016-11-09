;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 joo)

;; more-oo-inspired syntax on top of joo


;; (jinterface name . forms) and (jclass name . forms)

;; where forms are modded directly? No, still use define-syntax, so,
;; the toplevel jinterface / jclass definitions don't inherit, the
;; local redefinitions of those macros do.

;; Ah uh eh, can't use ##define-syntax as it escapes (begin )
;; forms. And I have to use begin forms since non-macro definitions
;; *do* have to escape it. Gah. Back to parsing myself. Forever.

;; So there are still two implementations of the jinterface and jclass
;; forms: the outmost, and the inner ones. Should they be called
;; differently even? Can't really do that right, forgetting etc. (A
;; whole language? Well yes but triggered still and so whole language
;; parser hence forever.)

;; Both implementations do the same thing though, they walk the direct
;; inner layer of forms and run the same expander (recursively) on
;; found forms.

;; Ah, interesting: *because* of ##define-syntax exiting begin scopes,
;; I now also have to *move* nested jclass / jinterface forms out to
;; the toplevel, wow. Ah won't actually help for the same
;; reason. *Wow*. Eh ok it will actually help, not because of the
;; nesting change but because the ordering is changed (and in line
;; with the original nesting, I mean stuff remains with stuff now).
;; (Ok, XX clean up those stupid comments some time.)


;; XX move to joo
(def (joo-extends-or-implements stx super-is-class? is-class?)
     (if super-is-class?
	 (if is-class?
	     `extends:
	     (source-error stx "an interface cannot extend a class"))
	 (if is-class?
	     `implements:
	     `extends:)))


(def (jclass:expand stx
		    is-class?
		    args
		    maybe-super-name
		    super-is-class?)
     (let-pair
      ((decl forms) args)
      (let ((c (lambda (name _maybe-constructor-name _field-decls)
		 (let ((forms* (map (C jclass:perhaps-expand-in-context
				       _
				       name
				       is-class?)
				    forms)))
		   `(begin
		      ,(sourcify
			`(,(if is-class? `joo-class `joo-interface)
			  ,decl
			  ,@(if maybe-super-name
				(list (joo-extends-or-implements
				       stx super-is-class? is-class?)
				      maybe-super-name)
				'())
			  ;; forms that are unrelated (didn't expand)
			  ;; remain in the scope of the joo-* definition:
			  ,@(map fst (filter (complement snd) forms*)))
			stx)
		      ;; forms that are expanded by jclass.scm are put
		      ;; afterwards:
		      ,@(map fst (filter snd forms*)))))))
	(joo:parse-decl decl c c c))))


(def (jclass:perhaps-expand-in-context expr ;; *should* always match |source?|
				       maybe-super-name
				       super-is-class?)
     -> (values-of any?
		   ;; #t if it did expand
		   boolean?)

     (mcase expr
	    (pair?
	     (let-pair ((a r) (source-code expr))
		       (if (pair? r)
			   (case (source-code a)
			     ;; wow have to check for expansion#
			     ;; version, too. OMG
			     ((jinterface expansion#jinterface) 
			      (values (jclass:expand expr
						     #f r
						     maybe-super-name
						     super-is-class?)
				      #t))
			     ((jclass expansion#jclass)
			      (values (jclass:expand expr
						     #t r
						     maybe-super-name
						     super-is-class?)
				      #t))
			     (else (values expr #f)))
			   (source-error expr "missing decl"))))
	    (else
	     (values expr #f))))

(def (jclass:toplevel-expand stx is-class?)
     (letv ((stx* did?) (jclass:perhaps-expand-in-context stx #f is-class?))
	   (if did?
	       stx*
	       (source-error stx "BUG"))))

(defmacro (jinterface decl . forms)
  (jclass:toplevel-expand stx #f))

(defmacro (jclass decl . forms)
  (jclass:toplevel-expand stx #t))

;; ^ XX btw double extends: or implements: keywords, how to handle?
;; Really \SCHEME[keyword arguments should handle duplicate argument
;; case generally]?


(TEST
 > (jinterface jct
	       (jclass (jclass_foo x y)
		       (jclass (bar z)
			       (jclass (baz)))
		       ;; method *after* an inner class definition to
		       ;; test for the ##define-syntax (inner syntax
		       ;; scoping) issue:
		       (def-method (meth s)
			 (+ (.x s) (.y s))))
	       (jinterface jct2
			   (jclass (jclass_foo2 x))))
 > (jclass_foo 10 12)
 #((jclass_foo) 10 12)
 > (def b (baz 10 12 13))
 > b
 #((baz) 10 12 13)
 > (jclass_foo? b)
 #t
 > (jct? b)
 #t
 > (jct2? b)
 #f
 > (jct? (jclass_foo2 1))
 #t
 > (jct2? (jclass_foo2 1))
 #t
 ;; > (jct? jct2) ehr there's no constructor. Do or do I not
 ;; have a way to check this hierarchy just on the class level yet?

 ;; > (with-exception-handler
 ;;    identity ;; source-error-message
 ;;    (& (eval (quote-source (jinterface jct
 ;; 				       (jclass (foo x y)
 ;; 					       (jinterface jctdeep
 ;; 							   (jclass (bar z)
 ;; 								   (jclass (baz)))))
 ;; 				       (jinterface jct2
 ;; 						   (jclass (foo2 x))))))))
 ;;"an interface cannot extend a class"

 ;; ^ ok wow that crashes Gambit; without with-exception-handler it's
 ;; fine, goes into debugger. Same thing when using quote.

 ;; Test the inner syntax scoping thing:
 > (expansion#jclass (foo x y) (jclass (bar z)))
 (begin (joo-class (foo x y))
	(begin (joo-class (bar z) extends: foo)))
 > (expansion#jclass (foo x y)
		     (jclass (bar z) (def-method (meth s) 'bar))
		     (def-method (meth s) 'foo))
 (begin
   (joo-class (foo x y)
	      (def-method (meth s) 'foo))
   (begin
     (joo-class (bar z) extends: foo
		(def-method (meth s) 'bar))))

 ;; and on the earlier, actually executed, definition of foo:
 > (.meth (jclass_foo 10 11))
 21)
