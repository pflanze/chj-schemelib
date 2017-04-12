;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(define-struct. timepoint
  t
  msg)

(define. (timepoint.seconds t)
  (time->seconds (.t t)))

(define *timepoints* '())

(define (timepoints-init!)
  (set! *timepoints* '()))

(define (timepoint! msg)
  (set! *timepoints* (cons (timepoint (current-time) msg)
			   *timepoints*)))

(define (timepoints)
  ;; seems to be usable till microsecond resolution? [h limited by
  ;; double number format?]
  (define format (cut inexact.number-format <> <> 6))
  (let* ((tps (reverse *timepoints*))
	 (firstt (.seconds (car tps))))
    (let ((lastt (fold-right (lambda (tp lastt)
			       (let ((t (.seconds tp)))
				 (println (format t 0)
					  "\t"
					  (format (- t lastt) 3)
					  "\t"
					  (.msg tp))
				 t))
			     firstt
			     *timepoints*)))
      (- lastt firstt))))

