;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require
 )

;; a better exception handler/catcher, to allow rethrow from original
;; context.

;; with-exception-handler/catcher too long?

;; (BTW inconsistency with my with-gensym etc macros. Consistent with
;; with-input-from-file etc. though. Well I knew this when I started
;; writing these macros, ok?)

(define (with-exceptions-to handler thunk)
  (continuation-capture
   (lambda (outer-c)
     (parameterize ((current-exception-handler
		     (lambda (e)
		       (continuation-capture
			(lambda (inner-c)
			  (continuation-graft
			   outer-c
			   (lambda ()
			     (let ((outer-handler (current-exception-handler)))
			       (handler e (lambda (e)
					    (continuation-graft inner-c
								outer-handler
								e)))))))))))
		   (thunk)))))

(TEST
 > (with-exceptions-to
    (lambda (e rethrow)
      (if (divide-by-zero-exception? e) 'div-by-zero (rethrow e)))
    (& (/ 1 0) (error 'bar)))
 div-by-zero

 ;; rethrow:
 > (%try-error
    (with-exceptions-to
     (lambda (e rethrow)
       (if (divide-by-zero-exception? e) 'div-by-zero (rethrow e)))
     (& (error 'bar) (/ 1 0))))
 #(error bar)

 ;; correct context?:
 > (define t (make-parameter 1))
 > (call/cc
    (lambda (exit)
      (with-exception-handler
       (lambda (e)
	 (exit (list e (t))))
       (& (with-exceptions-to
	   (lambda (e rethrow)
	     (if (divide-by-zero-exception? e)
		 'div-by-zero
		 (rethrow (error-exception->structure e))))
	   (& (parameterize ((t 2))
			    (error 'bar) (/ 1 0))))))))
 (#(error bar) 2)
 ;; (The exception is rethrown from the original exception context,
 ;; but the handler that decides to do so is run in the outer
 ;; context.)

 ;; test that no loop is occurring:
 > (%try-error (with-exceptions-to
		(lambda (e rethrow) (if (error 'foo) 'div-by-zero (rethrow e)))
		(& (/ 1 0))))
 #(error foo)
 )

