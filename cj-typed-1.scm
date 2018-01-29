;;; Copyright 2010-2014 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (fallible-1 fallible? fallible-string)
	 (cj-env-1 scm:object->string))


;; mostly-COPY from cj-warn to avoid circular dependency
(define cj-typed#warn
  (lambda (msg . objs)
    (let ((port (current-error-port))
	  (separator " "))
      (display msg port)
      (let lp ((objs objs))
	(cond ((null? objs)
	       (newline ;; newline/hooks
		port))
	      ((pair? objs)
	       (display separator port)
	       (display (scm:object->string (car objs)) port)
	       (lp (cdr objs)))
	      (else (error "improper list:" objs)))))))




(define (cj-typed#_type-check-error error)
  (lambda (use-source-error? maybe-exprstr predstr w v)
    ;; v = value
    ;; w = result of predicate

    (let ((err (lambda strs
		 (let ((msg (apply string-append
				   (if maybe-exprstr
				       (string-append maybe-exprstr " "))
				   "does not match "
				   predstr
				   strs)))
		   (if use-source-error?
		       (source-error v msg)
		       (error (string-append msg ":") v))))))
      (cond ((eq? w #f)
	     (err))
	    ((fallible? w)
	     (err " " (fallible-string w)))
	    (else
	     (error "predicate "
		    predstr
		    " returned invalid non-boolean value:"
		    w))))))

(define cj-typed#type-check-error
  (cj-typed#_type-check-error error))


;; Handling warning-only and ignore modes:

(define (type-failure-handling? v)
  (case v ((error warn ignore) #t)
	(else #f)))

(define current-type-failure-handling
  (make-parameter 'error))

(define cj-typed#_type-check-warn
  (cj-typed#_type-check-error cj-typed#warn))

(define (cj-typed#type-check-warn use-source-error? maybe-exprstr predstr w v)
  (case (current-type-failure-handling)
    ((warn)
     (continuation-capture
      (lambda (c)
	(let ((p (current-error-port)))
	  (write c p)
	  (display " " p))
	(cj-typed#_type-check-warn use-source-error? maybe-exprstr predstr w v)
	;; signal to the code from type-check-expand that the failure was
	;; handled:
	#t)))
    ((ignore)
     ;; claim that the failure was handled:
     #t)
    (else
     ;; signal to the code from type-check-expand that the failure was
     ;; not handled:
     #f)))



