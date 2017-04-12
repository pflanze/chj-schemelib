;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 constants)


(define-inline (square@ x)
  (declare (flonum) (not safe))
  (fl* x x))

(define-inline (freqmod@ x)
  ;; 0..1 -> 0..1
  (declare (flonum) (not safe))
  (let* ((min (CONST (log 0.1)))
	 (max (CONST (log 1.1)))
	 (x* (log (+ x 0.1))))
    (/ (- x* min) (- max min))))


(CONSTANTS)
