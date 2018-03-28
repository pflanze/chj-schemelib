;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (simple-match-1 warn*)
	 (cj-env object->serial-number-string))

(export current-debug
	(macro variables)
	(macro WARN))


(defmacro (variables . vs)
  (list 'quasiquote
	(fold-right (lambda (v rest)
		      (cons (symbol->keyword (source-code v))
			    (cons (list 'unquote v)
				  rest)))
		    '()
		    vs)))

(TEST
 > (let ((a 1) (b "hey")) (variables a b))
 (a: 1 b: "hey"))


(defenum WARN-mode
  warn-only
  warn/continuation)

;; #f turning warnings off, or a WARN-mode
(defparameter current-WARN warn/continuation)

(def (warn-plus:_WARN loc opt message args)
     (let* ((cont (lambda (msg)
		    (apply location-warn loc msg args))))
       (if (eq? opt 'warn/continuation)
	   (continuation-capture
	    (lambda (c)
	      (cont (string-append
		     "#"
		     (object->serial-number-string c)
		     " -- "
		     message))))
	   (cont message))))

(defmacro (WARN message . args)
  (let ((loc (source-location stx)))
    (with-gensym
     OPT
     `(##cond ((current-WARN)
	       => (lambda (,OPT)
		    (warn-plus:_WARN ',loc ,OPT ,message (##list ,@args))))))))

