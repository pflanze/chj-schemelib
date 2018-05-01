;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 cut
	 cj-typed
	 (simple-match-1 assert*)
	 (cj-symbol with-gensyms)
	 (cj-env define-if-not-defined symbol-append)
	 (srfi-1 any)
	 test)


(define (symbols.predicate syms)
  (lambda (v)
    (and (symbol? v)
	 (any (cut eq? <> v)
	      syms))))

(define-macro* (define-enum name sym . syms)
  (let ((syms (cons sym syms)))
    (assert*
     symbol? name
     (lambda (name)
       (with-gensyms
	(V SUCCESS FAIL)
	(let ((IF-PARSE (symbol-append "string.if->" name))
	      (name? (symbol-append name "?"))
	      (name-members (symbol-append name ":members"))
	      (name-members-vector (symbol-append name ":members-vector")))
	  `(begin
	     ,@(map (lambda (sym)
		      `(define-if-not-defined ,sym ',sym))
		    syms)
	     (define ,name-members
	       ',syms)
	     (define ,name-members-vector
	       ',(list->vector syms))
	     (define (,(symbol-append name "-ref") i)
	       (vector-ref ,name-members-vector i))
	     (define ,name?
	       (symbols.predicate ,name-members))
	     (define (,IF-PARSE ,V ,SUCCESS ,FAIL)
	       (cond ,@(map (lambda (sym)
			      (assert* symbol? sym
				       (lambda (sym)
					 `((string=? ,V ,(symbol->string sym))
					   (,SUCCESS ',sym)))))
			    syms)
		     (else
		      (,FAIL))))
	     (define (,(symbol-append "string.maybe-" name) ,V)
	       (,IF-PARSE ,V
			  identity
			  false/0))
	     (define (,(symbol-append "string." name) ,V)
	       (,IF-PARSE ,V
			  identity
			  (thunk
			   (error "string does not map to any member of enum:"
				  ',name
				  ,V))))
	     ;; Type safe 'methods' (avoiding the need to use any
	     ;; symbol functions directly):
	     (define-typed (,(symbol-append name ".string") #(,name? s))
	       (symbol->string s))
	     (define-typed (,(symbol-append name "-eq?") #(,name? a) #(,name? b))
	       (eq? a b)))))))))

(TEST
 > (define-enum ans Oui May Non)
 > (ans-ref 0)
 Oui
 > (ans-ref 1)
 May
 > (ans-ref 2)
 Non
 > (with-exception-catcher range-exception? (lambda () (ans-ref 3)))
 #t)

