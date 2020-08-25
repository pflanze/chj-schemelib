;;; Copyright 2018-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 ;; (cj-source-util schemedefinition-arity:pattern->template)
         )

(export scheme-parse:maybe-lambda-binds
        scheme-parse:lambda?
        scheme-parse:maybe-lambda-exact-arity
        )

(include "cj-standarddeclares.scm")


"Parsers for s-expressions representing Scheme code."

;; tests see scheme-parse-test.scm


(define (scheme-parse:maybe-lambda-binds v)
  (let ((v (source-code v)))
    (and (pair? v)
	 (let ((h (source-code (car v))))
	   (and (or (eq? h 'lambda)
		    (eq? h '##lambda))
		(let ((r (cdr v)))
		  (and (pair? r)
		       (car r))))))))

(define (scheme-parse:lambda? v)
  (and (scheme-parse:maybe-lambda-binds v) #t))


(define (scheme-parse:maybe-lambda-exact-arity v)
  (cond ((scheme-parse:maybe-lambda-binds v)
	 => (lambda (binds)
	      ;; wow need to strip source info here
	      (let ((t (schemedefinition-arity:pattern->template
			(source-code binds))))
		(and (eq? (vector-ref t 0) 'exact)
		     (vector-ref t 1)))))
	(else #f)))

