;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(define (with-output-to-string thunk)
  (call-with-output-string
   ""
   (lambda (port)
     (parameterize ((current-output-port port))
		   (thunk)))))

(define-macro* (*with-output-to-string expr)
  `(with-output-to-string (lambda ()
			    ,expr)))

(TEST
 > (*with-output-to-string (begin (display "Hello ") (display "World")))
 "Hello World")
