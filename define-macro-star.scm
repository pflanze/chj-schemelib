;;; Copyright 2010, 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A define-macro like macro definition mechanism that offers the
;; following functionality over define-macro:

;; - the inputs of the macro transformers are source objects, meaning
;; objects containing both source code and location information; the
;; macro transformers may return a mix of source objects and normal
;; objects without location information. Like with define-macro,
;; errors in latter objects are shown as occurring in the macro call
;; form.

;; - a variable |stx| is being introduced unhygienically, which holds
;; the source code of the whole macro call; this can be used to get
;; the location of the call form, for example.

;; - the macro transformer is made available at runtime (it can be
;; used from newly entered/loaded code, from a compiled binary,
;; without re-including macro definitions first)

;; - a modified macro is being put at expansion#the-macro-name, that
;; outputs the expanded code (with location information stripped), for
;; interactive debugging purposes.

;; In a second step, define-macro* itself is also being defined as a
;; define-macro* so that it is available at runtime.


;; quoted forms (since not normally defined yet) for require.scm
'(require cj-source) ;; and cj-source-util--include, well
'(export define-macro*)

; (compile-time
;  (include "../../lib/cj-warn.scm"))

(##define-syntax
 include-and-compiletimeload
 (lambda (stx)
   (cj-sourcify-deep
    (apply
     (lambda (_name relpath)
       (let* ((loc (source-location relpath))
	      (container (location-container loc))
	      (container-path
	       (if (with-exception-catcher
		    (lambda (e) #f) 
		    (lambda () (eval '##container->path-hook)))
		   (##container->path-hook container)
		   container))
	      (relpath (source-code relpath)))
	 (or (string? container-path)
	     (error "only works in files, not:" container container-path))
	 (let* ((path (path-normalize relpath
				      #f
				      (path-directory container-path)))
		(code `(begin
			 ;; HACK: drop require and TEST forms
			 (define-macro (require . x)
			   `(begin))
			 (define-macro (TEST . x)
			   `(begin))
			 (include ,path))))
	   (eval code)
	   code)))
     (source-code stx))
    stx)))

(include-and-compiletimeload "cj-source-util--include.scm")

(define-macro (both-times . body)
  (let ((code
	 `(begin
	    ,@body)))
    (eval code)
    code))

;; The following needs to be available both at compile as well as
;; runtime, too; actually we can even afford for it to be
;; re-initialized at runtime.

(both-times
 (define define-macro-star-cte
   (make-table test: eq?))

 (define (define-macro-star-maybe-ref sym)
   (table-ref define-macro-star-cte sym #f))

 (define (define-macro-star-set! sym expander)
   (table-set! define-macro-star-cte sym expander)))

(define (macro-star-expand-1 form)
  (let ((form* (source-code form)))
    (if (pair? form*)
	(let ((a (source-code (car form*))))
	  (if (symbol? a)
	      (cond ((define-macro-star-maybe-ref a)
		     => (lambda (expand)
			  (expand form)))
		    (else
		     form))
	      form))
	form)))

(define (macro-star-expand form)
  (let ((form* (macro-star-expand-1 form)))
    (if (eq? form form*)
	form
	(macro-star-expand form*))))


(##define-syntax
 define-macro*
 (lambda (stx)
   (let ()
     (define (pp-through msg x)
       ;;(warn msg (desourcify x))
       x)
     (define (optional-object? x) (eq? x '#!optional))
     (define (key-object? x) (eq? x '#!key))
     (define (rest-object? x) (eq? x '#!rest))
     (define (symbol-prefix prefix-str sym)
       (string->symbol
	(string-append prefix-str
		       (symbol->string
			(source-code sym)))))
     (let ((res
	    (cj-sourcify-deep
	     (apply
	      (lambda (macroname name+args . body)
		(let* ((name+args (##source-code name+args))
		       (name (car name+args))
		       (args (cdr name+args))
		       (code
			`(lambda (stx) ;; make stx visible?
			   (safer-apply
			    ',(schemedefinition-arity:pattern->template
			       (cons 'the-macronamer
				     (source/clean-dsssl-meta-objects args)))
			    (lambda (macroname ,@args)
			      ,@body)
			    (source/clean-keywords stx)
			    (lambda (msg)
			      (source-error stx msg))
			    (lambda (v)
			      (cj-sourcify-deep v stx)))))
		       (normal-expander-name
			(symbol-prefix "expander#" name)))
		  ;;(warn "define-macro* for: " (source-code name))
		  (pp-through "define-macro* result"
			      `(begin
				 (define ,normal-expander-name ,code)
				 ;; save for custom macro expansion, runtime:
				 (define-macro-star-set! ',name
				   ,normal-expander-name)
				 ;; both ##top-cte-add-macro! for runtime:
				 (##top-cte-add-macro!
				  ##interaction-cte
				  ',name
				  (##make-macro-descr
				   #t
				   -1
				   ,normal-expander-name
				   #f))
				 ;; and ##define-syntax for compile time:
				 (##define-syntax
				  ,name
				  ,code)))))
	      (source-code stx))
	     stx)))
       ;; for compile time:
       (eval res)
       ;; for run time:
       res))))

(both-times
 ;; whether to generate code that doesn't create runtime macros and
 ;; isn't stored in the global scope, callback to instead feed
 ;; name+expander to.
 (define define-macro*-maybe-local-callback
   (make-parameter #f)))

(define-macro* (define-macro* name+args . body)
  (define (pp-through msg x)
    ;;(warn msg (desourcify x))
    x)
  (define (optional-object? x) (eq? x '#!optional))
  (define (key-object? x) (eq? x '#!key))
  (define (rest-object? x) (eq? x '#!rest))
  (define (symbol-prefix prefix-str sym)
    (string->symbol
     (string-append prefix-str
		    (symbol->string
		     (source-code sym)))))
  (let ((res
	 (let* ((name+args (##source-code name+args))
		(name (car name+args))
		(args (cdr name+args))
		(apply-code
		 `(safer-apply
		   ',(schemedefinition-arity:pattern->template
		      (cons 'the-macroname
			    (source/clean-dsssl-meta-objects args)))
		   (lambda (macroname ,@args)
		     ,@body)
		   (source/clean-keywords stx)
		   (lambda (msg)
		     (source-error stx msg))
		   (lambda (v)
		     (cj-sourcify-deep v stx))))
		(normal-code
		 `(lambda (stx)	;; make stx visible?
		    ,apply-code))
		(expansion-code
		 `(lambda (stx)	;; make stx visible?
		    (list 'quote
			  (cj-desourcify
			   ,apply-code))))
		(normal-expander-name
		 (symbol-prefix "expander#" name))
		(maybe-local-callback
		 (define-macro*-maybe-local-callback)))
	   ;;(warn "define-macro* for: " (source-code name))
	   (if maybe-local-callback
	       (maybe-local-callback (source-code name)
				     (eval normal-code)))
	   (pp-through
	    "define-macro* result"
	    `(begin
	       (define ,normal-expander-name ,normal-code)
	       ,@(if maybe-local-callback
		     '()
		     `(;; save for custom macro expansion, runtime:
		       (define-macro-star-set! ',name ,normal-expander-name)
		       ;; both ##top-cte-add-macro! for runtime:
		       (##top-cte-add-macro!
			##interaction-cte
			',name
			(##make-macro-descr
			 #t
			 -1
			 ,normal-expander-name
			 #f))))
	       ;; and ##define-syntax for compile time:
	       (##define-syntax
		,name
		,normal-code)

	       ;; for debugging, introspection: a macro that returns
	       ;; the result quoted
	       ,@(if maybe-local-callback
		     '()
		     `(;; both ##top-cte-add-macro! for runtime:
		       (##top-cte-add-macro!
			##interaction-cte
			',(symbol-prefix "expansion#" name)
			(##make-macro-descr
			 #t
			 -1
			 ,expansion-code
			 #f))
		       ;; and ##define-syntax for compile time:
		       (##define-syntax
			,(symbol-prefix "expansion#" name)
			,expansion-code))))))))
    ;; for compile time:
    (eval res)
    ;; for run time:
    res))

;; Gambit work around aborting error handler during macro expansion:
;; "define-macro*-debug"
(define-macro* (define-macro*d name+args . body)
  `(define-macro* ,name+args
     (with-exception-handler
      ##primordial-exception-handler
      (lambda ()
	,@body))))




;; dummy-module

;; now put directly here to avoid having to special-case it (which
;; seems to be broken or was forgotten)

(define-macro* (require . body)
  `(begin))

(define-macro* (export . forms)
  '(begin))

