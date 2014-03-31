;;; Copyright 2013, 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro* cut)


(define (symbols.predicate syms)
  (lambda (v)
    (and (symbol? v)
	 (any (cut eq? <> v)
	      syms))))

(define-macro* (define-enum name . syms)
  (assert*
   symbol? name
   (lambda (name)
     (with-gensyms
      (V SUCCESS FAIL)
      (let ((IF-PARSE (symbol-append "string.if->" name)))
	`(begin
	   (define ,(symbol-append name "?")
	     (symbols.predicate ',syms))
	   (define (,IF-PARSE ,V ,SUCCESS ,FAIL)
	     (cond ,@(map (lambda (sym)
			    (assert* symbol? sym
				     (lambda (sym)
				       `((string=? ,V ,(symbol.string sym))
					 (,SUCCESS ',sym)))))
			  syms)
		   (else
		    (,FAIL))))
	   (define (,(symbol-append "string.maybe-" name) ,V)
	     (,IF-PARSE ,V
			identity
			false/0))
	   (define (,(symbol-append "string." name) ,V)
	     (,IF-PARSE ,V
			identity
			(thunk
			 (error "string does not map to any enum element of:"
				',name
				,V))))))))))

