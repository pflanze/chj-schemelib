;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 simple-match
	 cj-source-quasiquote
	 (cj-source-wraps source:symbol-append source.symbol?)
	 (cj-env *if-symbol-value)
	 test)


;; this now (unlike define-inline in cj-inline-1!) generates 3
;; versions: foo-inline macro, foo-lambda macro, and foo as a normal
;; function.
(define-macro* (define-inline name+vars body0 . body)
  (match-list*
   name+vars
   ((name . vars)
    (let* ((lambdacode
	    `(lambda ,vars
	       ,body0 ,@body))
	   (templatecode
	    `(,lambdacode
	      ,@(map (lambda (v)
		       (list 'unquote v))
		     vars))))
      (quasiquote-source
       (begin
	 (define ,name
	   ,lambdacode)
	 (define-macro* (,(symbol-append (source-code name) '-lambda))
	   ,(list 'quasiquote-source lambdacode))
	 (define-macro* (,(symbol-append (source-code name) '-inline) ,@vars)
	   ,(list 'quasiquote-source templatecode))))))))

;; XX just how dangerously unsafe is this?
(define-macro* (inline-through-decompile proc)
  (let ((v (eval proc)))
    (if (procedure? v)
	(##decompile v)
	(source-error proc
		      "does not evaluate to a procedure"))))

(define-macro* (inline proc)
  (let ((idec (lambda () `(inline-through-decompile ,proc))))
    (cond ((source.symbol? proc)
	   (let ((proclambda (source:symbol-append proc '-lambda)))
	     ;; XX could go safer than this by storing inline
	     ;; definitions from define-inline
	     (cond ((define-macro-star-maybe-ref (symbol-append (source-code proc) '-inline))
		    => (lambda (_)
			 `(,proclambda)))
		   (else
		    (idec)))))
	  (else
	   (idec)))))


(TEST
 > (define-inline (xssjijqtcs n) (* n n))
 > (expansion#inline xssjijqtcs)
 (xssjijqtcs-lambda)
 > (expansion#inline inc)
 (inline-through-decompile inc))

