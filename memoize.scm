;;; Copyright 2015 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(define (memoize f #!optional (table (make-table)))
  (let ((nothing (cons 1 2)))
    (lambda vs
      (let ((v? (table-ref table vs nothing)))
	(if (eq? v? nothing)
	    (let ((v (apply f vs)))
	      ;; XX: make multi-threading safe
	      (table-set! table vs v)
	      v)
	    v?)))))

