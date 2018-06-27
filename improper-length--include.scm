;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require)

(define (improper-length v)
  ;; copy from cj-env because of phasing issue
  (define (inc x)
    (+ x 1))
  ;; /copy
  (let lp ((v v)
	   (l 0))
    (cond ((pair? v)
	   (lp (cdr v)
	       (inc l)))
	  ((null? v)
	   l)
	  (else
	   (- (inc l))))))

;; tests see improper-length-test.scm
