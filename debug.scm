;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.

(require easy)

(export (macro DEBUG)
	(macro T))


(def *debug?* #t)


(defmacro (DEBUG . args)
  (if *debug?*
      `(warn ,@args)
      `(##void)))


;; "trace"
(defmacro (T . args)
  (if *debug?*
      (with-gensym
       res
       (let ((vs (map (comp gensym .string) (cdr (iota (length args))))))
	 `(let ,(map (lambda (v arg)
		       `(,v ,arg))
		     vs
		     (cdr args))
	    (warn "T:"
		  ;;,(object->string (cj-desourcify (car args)))
		  (list
		   ',(car args)
		   ,@(map (lambda (v)
			    `(.show ,v))
			  vs))
		  '->
		  '...)
	    (let ((,res (,(car args) ,@vs)))
	      (warn " :"
		    ;;,(object->string (cj-desourcify (car args)))
		    (list
		     ',(car args)
		     ,@(map (lambda (v)
			      `(.show ,v))
			    vs))
		    '->
		    (.show ,res))
	      ,res))))
      args))

