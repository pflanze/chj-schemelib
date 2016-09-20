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
		 `(,(if is-class? `joo-class `joo-interface)
		   ,decl
		   ,@(if maybe-super-name
			 (list (joo-extends-or-implements
				stx super-is-class? is-class?)
			       maybe-super-name)
			 '())
		   ,@(map (C jclass:perhaps-expand-in-context
			     #f
			     _
			     name
			     is-class?)
			  forms)))))
	(joo:parse-decl decl c c c))))


(def (jclass:perhaps-expand-in-context require-match?
				       expr
				       maybe-super-name
				       super-is-class?)
     (mcase expr
	    (pair?
	     (let-pair ((a r) (source-code expr))
		       (if (pair? r)
			   (case (source-code a)
			     ;; wow have to check for expansion#
			     ;; version, too. OMG
			     ((jinterface expansion#jinterface) 
			      (jclass:expand expr
					     #f r
					     maybe-super-name
					     super-is-class?))
			     ((jclass expansion#jclass)
			      (jclass:expand expr
					     #t r
					     maybe-super-name
					     super-is-class?))
			     (else expr))
			   (source-error expr "missing decl"))))
	    (else
	     (if require-match?
		 (source-error expr "BUG")
		 expr))))

(defmacro (jinterface decl . forms)
  (jclass:perhaps-expand-in-context #t stx #f #f))

(defmacro (jclass decl . forms)
  (jclass:perhaps-expand-in-context #t stx #f #t))

;; ^ XX btw double extends: or implements: keywords, how to handle?
;; Really \SCHEME[keyword arguments should handle duplicate argument
;; case generally]?


(TEST
 > (jinterface jclasst
	       (jclass (foo x y)
		       (jclass (bar z)
			       (jclass (baz))))
	       (jinterface jclasst2
			   (jclass (foo2 x))))
 > (foo 10 12)
 #((foo) 10 12)
 > (def b (baz 10 12 13))
 > b
 #((baz) 10 12 13)
 > (foo? b)
 #t
 > (jclasst? b)
 #t
 > (jclasst2? b)
 #f
 > (jclasst? (foo2 1))
 #t
 > (jclasst2? (foo2 1))
 #t
 ;; > (jclasst? jclasst2) ehr there's no constructor. Do or do I not
 ;; have a way to check this hierarchy just on the class level yet?

 ;; > (with-exception-handler
 ;;    identity ;; source-error-message
 ;;    (& (eval (quote-source (jinterface jclasst
 ;; 				       (jclass (foo x y)
 ;; 					       (jinterface jclasstdeep
 ;; 							   (jclass (bar z)
 ;; 								   (jclass (baz)))))
 ;; 				       (jinterface jclasst2
 ;; 						   (jclass (foo2 x))))))))
 ;;"an interface cannot extend a class"

 ;; ^ ok wow that crashes Gambit; without with-exception-handler it's
 ;; fine, goes into debugger. Same thing when using quote.
 )

