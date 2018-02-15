;;; Copyright 2010-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require table-1
	 (cj-env-1 scm:object->string))

(export warn
	displayln ;; move?
	#!optional
	port-add-hook!
	port-remove-hook!
	newline/hooks
	display/hooks
	displayln/hooks)


;; Move to sep lib?

(define cj-warn:hooks (make-table test: eq?
				  weak-keys: #t))

(define (port-add-hook! port proc)
  (table-push! cj-warn:hooks port proc))

(define (port-remove-hook! port proc)
  (let ((l (filter (lambda (h)
		     (not (eq? h proc)))
		   (table-ref cj-warn:hooks port '()))))
    (if (null? l)
	(table-set! cj-warn:hooks port)
	(table-set! cj-warn:hooks port l))))

(define (port-run-hooks! port)
  (if (not (zero? (table-length cj-warn:hooks))) ;; optim, fair?
      (for-each (lambda (proc)
		  ;; XX would I want to also pass the argument being printed?
		  (proc port))
		(table-ref cj-warn:hooks port '()))))

(define (newline/hooks p)
  (newline p)
  (port-run-hooks! p))

(define (display/hooks v p)
  (display v p)
  (port-run-hooks! p))

;; sep sep lib?
(define (displayln v p)
  (display v p)
  (newline p))
;;/sep sep lib

(define (displayln/hooks v p)
  (displayln v p)
  (port-run-hooks! p))


;;/ sep lib

(define (warn msg . objs)
  (let ((port (current-error-port))
	(separator " "))
    (display msg port)
    (let lp ((objs objs))
      (cond ((null? objs)
	     (newline/hooks port))
	    ((pair? objs)
	     (display separator port)
	     (display (scm:object->string (car objs)) port)
	     (lp (cdr objs)))
	    (else (error "improper list:" objs))))))
