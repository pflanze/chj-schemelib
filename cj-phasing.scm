;;; Copyright 2011-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star)

(export (macro insert-result-of)
	(macro compile-time)
	(macro both-times))

;; This used to be compile-time, which could be used both for the
;; result as well as the side effects. now be explicit about the
;; purpose:

(define-macro* (insert-result-of . body)
  (eval `(begin
	   ,@body)))


;; Note that macros that are available at runtime will need
;; the use of both-times, not compile-time!

(define-macro* (compile-time . body)
  (let ((code
	 `(begin
	    ,@body)))
    (eval code)
    `(void)))

(define-macro* (both-times . body)
  (let ((code
	 `(begin
	    ,@body)))
    (eval code)
    code))
