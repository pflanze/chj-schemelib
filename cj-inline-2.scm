;;; Copyright 2010, 2011, 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (define-macro-star)
	 (simple-match)
	 cj-source-quasiquote)

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

