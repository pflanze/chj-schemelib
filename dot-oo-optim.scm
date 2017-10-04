;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require easy
	 symbol-append
	 (simple-match-1 assert*)
	 dot-oo)

(export dot-oo-optim-for)


(def (dot-oo-optim-for-expand methodname num-args)
     (assert*
      symbol? methodname
      (lambda (methodname)
	(assert*
	 natural? num-args
	 (lambda (num-args)
	   (let ((ARGS (map (lambda_ (gensym)) (iota num-args)))
		 (generic-name (symbol-append methodname "/"
					      (number->string num-args))))
	     `(define (,generic-name
		       ,@ARGS)
		(cond ,@(map (lambda (entry)
			       (let ((type-name (car entry)))
				 `((,(symbol-append type-name "?") ,(car ARGS))
				   (,(symbol-append type-name methodname)
				    ,@ARGS))))
			     (dot-oo:show-method-table
			      (eval ;; <-- XX forever evil unsafe
			       (symbol-append "dot-oo-method-table#"
					      methodname))))
		      (else
		       (error ,(string-append (symbol->string generic-name)
					      ": no method for:")
			      ,(car ARGS)))))))))))



(defmacro (dot-oo-optim-for methodname num-args)
  (dot-oo-optim-for-expand methodname num-args))

