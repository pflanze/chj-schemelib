;;; Copyright 2013-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(defmacro (variables . vs)
  (list 'quasiquote
	(fold-right (lambda (v rest)
		      (cons (symbol->keyword (source-code v))
			    (cons (list 'unquote v)
				  rest)))
		    '()
		    vs)))

(defmacro (WARN . args)
  `(if *warn*
       (warn ,@args)))

(def *warn* #f)

