;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.simple-match))

;; XXX looses location information (during the pass through
;; define-macro[*] definition, probably)

(define-macro* (define-inline name+vars body0 . body)
  (match-list*
   name+vars
   ((name . vars)
    (let ((templatecode
	   `((lambda ,vars
	       ,body0 ,@body)
	     ,@(map (lambda (v)
		      (list 'unquote v)) vars))))
      (list 'define-macro* name+vars
	    (list 'quasiquote templatecode))))))

