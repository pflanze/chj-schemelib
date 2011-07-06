;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; provide delay force promise? no-delay force1

(define promise? ##promise?)
(define force1 ##force);; (not really the same thing! ##force does recursive force, so F* will not show nested <P>'s with this--use debuggable-promise if this is needed)

;; for debugging uncomment:
;(include "debuggable-promise.scm")


;; simple way to switch off a |delay|. And maybe makes for good
;; documentation of places where it's not clear whether delaying or
;; not is the right solution:
(define-macro* (no-delay expr)
  expr)

;; force variables
(define-macro* (FV vars* . body)
  (match-list* vars*
	       (vars
		`(let ,(map (lambda (var*)
			      ;;(match-list) no. still doesn't do symboltypechecksoranythinglikethat.
			      ;;(let ((var (symbol-))))
			      ;; well why test? just let that be handled by the next stage.
			      `(,var* (force ,var*)))
			    vars)
		   ,@body))))

