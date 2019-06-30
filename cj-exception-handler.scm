;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Wrap Gambit's primordial-exception-handler so that the exception
;; value can be accessed (e.g. to inspect it in a way that doesn't
;; shorten the message).

(require (cj-source source-error? show-source-error
		    location-string)
	 (predicates-1 any?)
	 dot-oo)

(export current-exception
	write-exception-message
	cj-exception-handler:activate!
	cj-exception-handler:activate-if-primordial!)


(define current-exception (make-parameter #f))

;; method that should return a list with the first item being a
;; message string without ":", a ": " is appended by the printer. And
;; then a list of values, optionally prepended by a keyword, which
;; will be written via write or pretty-print (i.e. no .show call is
;; being applied, OK?)
(define. (any.maybe-exception-message _) #f)

;; without a newline afterwards, i.e. do not wrap arond lines, OK?
(define (write-exception-message v #!optional (p (current-output-port)))
  (cond ((and (pair? v) (list? v))
	 (display (car v) p)
	 (display ": " p)
	 (let lp ((l (cdr v)))
	   (if (pair? l)
	       (let ((r (cdr l)))
		 (write (car l) p)
		 (if (pair? r)
		     (begin
		       (display " " p)
		       (lp r))
                     (void))))))
	((string? v)
	 (display v p))
	(else
	 ;; just a safe fallback, don't use, OK?
	 (write v p))))


(define (cj-exception-handler e)
  ;; Show every object with a serial number handle first, so that it
  ;; can be accessed as the original value.
  ;; (XX BAD: immediate objects are never freed from the serialization
  ;; table, right? But, shouldn't that be handled there instead?)
  (let ((p (console-port)))
    ;; right it's the console-port ?
    (display "cj-exception-handler: exception #" p)
    (display (object->serial-number e) p)
    (newline p)
    ;; (Call previous handler instead of the primordial one ?)
    (cond ((source-error? e)
	   (show-source-error e p)
	   (##repl))
	  ((.maybe-exception-message e)
	   => (lambda (msg)
		(continuation-capture
		 (lambda (cont)
		   (display "*** ERROR IN " p)
		   (cond ((##continuation-creator cont)
			  => (lambda (creator)
			      (display creator p)
			      (display ", " p))))
		   (display (location-string (##continuation-locat cont)) p)
		   (display " -- " p)
		   (write-exception-message msg p)
		   (newline p)
		   (##repl)))))
	  (else
	   (primordial-exception-handler e)))))


(define (cj-exception-handler:activate!)
  (current-exception-handler cj-exception-handler))

(define (cj-exception-handler:activate-if-primordial!)
  (if (eq? (current-exception-handler) primordial-exception-handler)
      (begin
	(cj-exception-handler:activate!)
	#t)
      #f))

(cj-exception-handler:activate-if-primordial!)
