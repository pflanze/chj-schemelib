;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star)

(export (macro named-lambda)
	(macro named))


(define-macro* (named-lambda name vars . body)
  `(letrec ((,name (lambda ,vars
		     ,@body)))
     ,name))


;; Simpler than named-lambda and combinable with other (special) forms:

(define-macro* (named name form)
  (assert* symbol? name
	   (lambda (_)
	     `(letrec ((,name ,form))
		,name))))


