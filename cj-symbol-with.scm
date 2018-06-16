;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 simple-match-1)

(export (macro with-gensyms)
	(macro with-gensym))

(include "cj-standarddeclares.scm")


(define-macro* (with-gensyms vars . body)
  (match-list*
   vars
   (vars
    `(let ,(map (lambda (v)
		  `(,v (gensym ',v)))
		vars)
       ,@body))))

(define-macro* (with-gensym var . body)
  `(with-gensyms (,var) ,@body))

