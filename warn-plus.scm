;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (simple-match-1 warn*))

(export current-debug
	(macro variables)
	(macro WARN))


(defmacro (vars . vs)
  (list 'quasiquote
	(cons 'vars
	      (fold-right (lambda (v rest)
			    (cons (symbol->keyword (source-code v))
				  (cons (list 'unquote v)
					rest)))
			  '()
			  vs))))

(TEST
 > (let ((a 1) (b "hey")) (vars a b))
 (vars a: 1 b: "hey"))


(defparameter current-WARN? #f)

(defmacro (WARN . args)
  `(if (current-WARN?)
       (warn* ,@args)))

