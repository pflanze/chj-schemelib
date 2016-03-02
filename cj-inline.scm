;;; Copyright 2010, 2011, 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 simple-match
	 cj-source-quasiquote
	 (cj-typed ->)
	 (cj-source-wraps source:symbol-append source.symbol?)
	 (cj-env *if-symbol-value)
	 test)

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
	 (define-macro* (,(symbol-append (source-code name) '-lambda))
	   ,(list 'quasiquote-source lambdacode))
	 (define-macro* ,name+vars
	   ,(list 'quasiquote-source templatecode))))))))

;; XX just how dangerously unsafe is this?
(define-macro* (inline-through-decompile proc)
  (##decompile (-> procedure? (eval proc))))

(define-macro* (inline proc)
  (let ((idec (lambda () `(inline-through-decompile ,proc))))
    (cond ((source.symbol? proc)
	   (let ((proclambda (source:symbol-append proc '-lambda)))
	     ;; XX could go safer than this by storing inline
	     ;; definitions from define-inline
	     (cond ((define-macro-star-maybe-ref (source-code proc))
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

