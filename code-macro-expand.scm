;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 symboltable)

(export macro-expand/symtbl
	macro-expander/symtbl)

(include "cj-standarddeclares.scm")


;; Makeshift "manual" ##let-syntax. Only top-level exprs are expanded!
;; (Should it be extended to enter ##begin forms?  Probably. Need to
;; know what |begin| is bound to, though. Forever.)

;; Expanders are found in symtbl (which has precedence) and
;; define-macro-star's table.

(define (macro-expand/symtbl expr symtbl)
  (let ((expr* (source-code expr)))
    (if (pair? expr*)
	(let ((a (source-code (car expr*))))
	  (cond ((and (symbol? a)
		      (or (symboltable-ref symtbl a #f)
			  ;; Need to expand other macros, too, to
			  ;; handle entries in their expansions!
			  (define-macro-star-maybe-ref a)))
		 => (lambda (expand)
		      ;; iterate until no macro expander found anymore
		      (macro-expand/symtbl (expand expr) symtbl)))
		(else
		 expr)))
	expr)))


(define (macro-expander/symtbl symtbl)
  (lambda (expr)
    (macro-expand/symtbl expr symtbl)))

