;;; Copyright 2014-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 (list-util improper-fold-right)
	 C)

(export improper*-map/tail
	improper*-map
	improper-every)

;; improper->proper-map

(define (improper*-map/tail fn v tail)
  (improper-fold-right (lambda (a r)
			 (cons (fn a) r))
		       tail
		       v))

(define improper*-map (C improper*-map/tail _ _ '()))

(TEST
 > (improper*-map true? '("" . ""))
 (#f #f))


(define (improper-every pred v)
  (cond ((pair? v)
	 (and (pred (car v))
	      (improper-every pred (cdr v))))
	((null? v)
	 #t)
	(else
	 (pred v))))
