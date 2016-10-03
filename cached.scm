;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.



;; Ehrm, I had something similar multiple times already, no?

(require)

(export cached/1)


(define (cached/1 fn)
  (let* ((t (make-table))
	 ;; heh: can re-use t itself as nothing value!
	 (nothing t))
    (lambda (v)
      (let ((r (table-ref t v nothing)))
	(if (eq? r nothing)
	    (let ((r (fn v)))
	      (table-set! t v r)
	      r)
	    r)))))

