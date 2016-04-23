;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

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


