;;; Copyright 2011-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (simple-match-1 match*)
	 (cj-source-util-2 assert)
	 test)

(export (macro expansion)
	(macro macro-expand-all)
	(macro macro-expand))


(define-macro* (expansion expr . exprs)
  (define CONT 'cont) ;; |...| looks nice, but may be confusing
  (let ((expr (if (pair? exprs)
		  (sourcify (cons expr exprs) stx)
		  expr)))
    (let ((code
	   (##decompile
	    (eval `(lambda ()
		     ,expr
		     ,CONT)))))
      (list
       'quote
       (match*
	code
	((LAMBDA _ _letrec)
	 _letrec)
	((LAMBDA _ form end)
	 (assert (equal? end CONT))
	 form))))))

(define-macro* (macro-expand-all expr)
  `(expansion ,expr))


;; NOTE: this (currently) only expands define-macro-star macros, not
;; Gambit's built-in ones. (Also, it doesn't (currently, again,
;; forever) care about the context.)

(define-macro* (macro-expand expr)
  `(quote ,(macro-star-expand expr)))

(TEST
 > (macro-expand (cons 1 2))
 (cons 1 2)
 > (macro-expand (macro-expand (cons 1 2)))
 '(cons 1 2)
 ;; It only expands the upper level, not deeply:
 > (macro-expand (macro-expand (cons 1 (macro-expand 2))))
 '(cons 1 (macro-expand 2))
 ;; Versus:
 > (macro-expand-all (macro-expand (cons 1 (macro-expand 2))))
 '(cons 1 (macro-expand 2))
 > (macro-expand-all (cons 1 (macro-expand 2)))
 (cons 1 2))

