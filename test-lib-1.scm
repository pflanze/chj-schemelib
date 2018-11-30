;;; Copyright 2006-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 cj-exception)


;; A library of helper functions for writing tests
;; also see test-random

;; Also see test-lib.scm, split only because of dependancy reasons.

(export (macro time-cpu)
	(macro %try-error)
	(macro %try)
	(macro %error?)
	(macro %try-syntax-error))

;; test cpu usage:

(define-macro* (time-cpu expr)
  `(_time-cpu (lambda ()
		,expr)))

;; test for exceptions:

(define (try-error-handler x-exception?
			   x
			   x-exception-message
			   x-exception-parameters)
  (lambda (e)
    (cond ((x-exception? e)
	   (apply vector
		  x
		  (x-exception-message e)
		  (x-exception-parameters e)))
	  (else
	   (raise e)))))

(define error-exception->structure
  (try-error-handler error-exception?
		     'error
		     error-exception-message
		     error-exception-parameters))


(define (%try-error-f thunk)
  (with-exception-catcher
   error-exception->structure
   thunk))

(define-macro* (%try-error form)
  `(%try-error-f (thunk ,form)))


(define (test-lib-1:try thunk)
  (with-exception/continuation-catcher
   (lambda (e)
     (list 'exception text: (exception/continuation-text e)))
   (lambda ()
     (list 'value (thunk)))))

(define-macro* (%try expr)
  `(test-lib-1:try (lambda ()
		     ,expr)))



(define (%error?-f thunk)
  (with-exception-catcher
   (lambda (e)
     #t)
   (lambda ()
     (vector 'not-an-error (thunk)))))

(define-macro* (%error? form)
  `(%error?-f (thunk ,form)))

;; and for syntax exceptions:

(define source-exception->structure
  (try-error-handler source-error?
		     'source-error
		     source-error-message
		     source-error-args ;; uh consistency?
		     ))

(define (%try-syntax-error-f thunk)
  (with-exception-catcher
   source-exception->structure
   thunk))

(define-macro* (%try-syntax-error form)
  `(%try-syntax-error-f (thunk (eval ',form))))

