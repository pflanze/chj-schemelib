;;; Copyright 2010, 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;;(require)  included in cj-source and mod/mod.scm


(define (vector-map-1 fn vec)
  ;;(list->vector (map fn (vector->list vec)))
  ;;COPY
  (define (inc x) (+ x 1))
  ;; /COPY
  (let* ((len (vector-length vec))
	 (res (make-vector len)))
    (let lp ((i 0))
      (if (= i len)
	  res
	  (begin
	    (vector-set! res i (fn (vector-ref vec i)))
	    (lp (inc i)))))))

