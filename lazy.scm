;;; Copyright 2010-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 simple-match
	 debuggable-promise
	 ;; well, just for convenience?:
	 lazy-1)

(export promise?
	(macro no-delay)
	(macro FV)
	;; re-export just for convenience:
	@promise-evaluated?
	@promise-value

	#!optional
	;; force1 ;; used in F*, but need to fix it in general.
	)

(possibly-use-debuggable-promise)


;; provide delay force promise? no-delay force1

;; (define promise? ##promise?)  now in debuggable-promise / debuggable-promise-everywhere

;; XX implement replacement?:
;; (define force1 ##force);; (not really the same thing! ##force does recursive force, so F* will not show nested <P>'s with this--use debuggable-promise if this is needed)


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

