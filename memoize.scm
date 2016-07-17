;;; Copyright 2015 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require)


(define memoize:nothing (gensym 'nothing))

(define (memoize f
		 #!optional
		 (table (make-table)))
  (lambda vs
    (let ((v? (table-ref table vs memoize:nothing)))
      (if (eq? v? memoize:nothing)
	  (let ((v (apply f vs)))
	    ;; XX: make multi-threading safe
	    (table-set! table vs v)
	    v)
	  v?))))

