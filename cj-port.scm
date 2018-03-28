;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test)

(export with-output-to-string
	with-error-to-string
	(macro *with-output-to-string)
	with-output-to-string*
	pretty-print-to-string)


(define (make-with-_-to-string current-_-port)
  (lambda (thunk)
    (call-with-output-string
     ""
     (lambda (port)
       (parameterize ((current-_-port port))
		     (thunk))))))

(define with-output-to-string
  (make-with-_-to-string current-output-port))

(define with-error-to-string
  (make-with-_-to-string current-error-port))


(define-macro* (*with-output-to-string expr)
  `(with-output-to-string (lambda ()
			    ,expr)))

(TEST
 > (*with-output-to-string (begin (display "Hello ") (display "World")))
 "Hello World")


(define (with-output-to-string* thunk)
  (let* ((result #f) ;; XX does this always work (with call/cc)?
	 (str (call-with-output-string
	       ""
	       (lambda (port)
		 (set! result (parameterize ((current-output-port port))
					    (thunk)))))))
    ;; (which result order?)
    (values result
	    str)))

(TEST
 > (values->vector (with-output-to-string* (& (print "hello") 1)))
 #(1 "hello"))


(define (pretty-print-to-string v)
  (with-output-to-string (lambda ()
			   (pretty-print v))))

