;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 symboltable)

(export macro-expand/symtbl
	macro-expander/symtbl
	begin-flatten)

(include "cj-standarddeclares.scm")


;; Makeshift "manual" ##let-syntax. Only top-level exprs are expanded!
;; (Should it be extended to enter ##begin forms?  Probably. Need to
;; know what |begin| is bound to, though. Forever.)

;; Expanders are found in symtbl (which has precedence) and
;; define-macro-star's table.

;; If a symtbl value is #t (instead of a function) then the given form
;; is left unchanged (this even disables top-level macros for the
;; given form).

;; This is only macroexpand, not macroexpand-all, *except* it also
;; enters (begin ) expressions and then expands all subexpressions,
;; recursively.

(define (macro-expand/symtbl expr symtbl)
  (let ((_expr (source-code expr)))
    (if (pair? _expr)
	(let ((a (source-code (car _expr))))
	  (if (or (eq? a 'begin) (eq? a '##begin))
	      ;; ^ use some begin predicate, forever
	      (cons a
		    (map (C macro-expand/symtbl _ symtbl)
			 (cdr _expr)))
	      
	      ;; expand the form at point
	      (cond ((and (symbol? a)
			  (or (symboltable-ref symtbl a #f)
			      ;; Need to expand other macros, too, to
			      ;; handle entries in their expansions!
			      (define-macro-star-maybe-ref a)))
		     => (lambda (expand)
			  ;; Check for special signal to disable a macro (is this a hack?)
			  (if (eq? expand #t)
			      expr
			      ;; iterate until no macro expander found anymore
			      (macro-expand/symtbl (possibly-sourcify (expand expr) expr)
						   symtbl))))
		    (else
		     expr))))
	expr)))

(define (macro-expander/symtbl symtbl)
  (lambda (expr)
    (macro-expand/symtbl expr symtbl)))


(TEST
 > (def t (symboltable* foo: (lambda (stx) `(bar ,@(cdr stx)))
			bar: (lambda (stx) `(b ,@(cdr stx)))))
 > (macro-expand/symtbl '(foo (bar)) t)
 (b (bar))
 > (macro-expand/symtbl '(begin (foo (bar))
				(##begin (beb (bar))
					 (bar (foo)))) t)
 (begin (b (bar))
	(##begin (beb (bar))
		 (b (foo)))))



;; note: always returns a list of expressions, not a begin form!
(define (begin-flatten expr tail)
  (let ((_expr (source-code expr)))
    (if (pair? _expr)
	(let ((a (source-code (car _expr))))
	  (case a
	    ((begin ##begin)
	     (fold-right begin-flatten tail (cdr _expr)))
	    (else (cons expr tail))))
	(cons expr tail))))

(TEST
 > (begin-flatten '1 '())
 (1)
 > (begin-flatten '(begin (bar) (begin (foo))) '())
 ((bar) (foo)))

